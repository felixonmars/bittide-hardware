#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_mut)]
#![allow(clippy::collapsible_if)]
#![no_main]

// #![feature(alloc_error_handler)]

#[cfg(not(test))]
use riscv_rt::entry;

mod soft_loopback;
mod axi_ethernet;
mod time;
mod axi_buffers;
use core::fmt::Write;
use bittide_sys::uart::Uart;
use bittide_sys::panic_handler::set_panic_handler_uart;
#[cfg(feature = "std")]
#[allow(dead_code)]
mod utils;

use core::str;

use axi_ethernet::AxiEthernet;
use smoltcp::iface::{Config, Interface, SocketSet};
use smoltcp::phy::Medium;
use smoltcp::socket::tcp;
use smoltcp::time::{Duration};
use smoltcp::wire::{EthernetAddress, IpAddress, IpCidr};

const UART_ADDR:usize = 0x4000_0000;
const TX_AXI_ADDR:usize = 0x6000_0000;
const RX_AXI_ADDR:usize = 0x8000_0000;
const TIME_ADDR:usize = 0xa000_0000;
const RX_BUFFER_SIZE:usize = 400 * 4;

#[cfg_attr(not(test), entry)]
fn main() -> !{
    // Initialize and test UART.
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    let mut panic_uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    unsafe{ set_panic_handler_uart(panic_uart)};

    // Initialize and test clock
    let mut clock = time::Clock::new(TIME_ADDR, 125*10^6);

    // Create interface
    let mut config = Config::new();
    let mut device = AxiEthernet::new(Medium::Ethernet, RX_AXI_ADDR as *mut u8, TX_AXI_ADDR as *mut u8, RX_BUFFER_SIZE);
    config.hardware_addr = Some(EthernetAddress([0x02, 0x00, 0x00, 0x00, 0x00, 0x01]).into());

    let mut iface: Interface = Interface::new(config, &mut device);
    iface.update_ip_addrs(|ip_addrs| {
        ip_addrs
            .push(IpCidr::new(IpAddress::v4(192, 168, 1, 101), 8))
            .unwrap();
    });

    // Create sockets
    let server_socket = {
        // It is not strictly necessary to use a `static mut` and unsafe code here, but
        // on embedded systems that smoltcp targets it is far better to allocate the data
        // statically to verify that it fits into RAM rather than get undefined behavior
        // when stack overflows.
        static mut TCP_SERVER_RX_DATA: [u8; RX_BUFFER_SIZE] = [0; RX_BUFFER_SIZE];
        static mut TCP_SERVER_TX_DATA: [u8; RX_BUFFER_SIZE] = [0; RX_BUFFER_SIZE];
        let tcp_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut TCP_SERVER_RX_DATA[..] });
        let tcp_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut TCP_SERVER_TX_DATA[..] });
        tcp::Socket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let mut sockets: [_; 1] = Default::default();
    let mut sockets = SocketSet::new(&mut sockets[..]);
    let server_handle = sockets.add(server_socket);

    let mut loopback_data: [u8; RX_BUFFER_SIZE] = [0; RX_BUFFER_SIZE];
    let mut loopback_data_length = 0;

    let mut socket = sockets.get_mut::<tcp::Socket>(server_handle);
    let mut last_state = socket.state();

    loop{
        iface.poll(clock.elapsed(), &mut device, &mut sockets);
        let mut socket = sockets.get_mut::<tcp::Socket>(server_handle);
        // Check if the remote has closed the connection.
        let mut current_state = socket.state();
        if last_state != current_state{
            // Print state changes.
            writeln!(uart, "{} : {} ms", current_state, clock.elapsed().total_millis()).unwrap();
            last_state = current_state;
        }
        // If the socket has received a FIN packet, close the socket from our side.
        if current_state == smoltcp::socket::tcp::State::CloseWait {
            writeln!(uart, "Remote closed connection, closing here too.").unwrap();
            writeln!(uart, "Received {} bytes, transmitted {} bytes.", device.bytes_received, device.bytes_sent).unwrap();
            device.bytes_sent = 0;
            device.bytes_received = 0;
            socket.close();
        }

        if !socket.is_active() && !socket.is_listening() {
            writeln!(uart,"listening").unwrap();
            socket.listen(7).unwrap();
        }

        if socket.is_active() && loopback_data_length == 0 && socket.can_recv() {
            loopback_data_length = socket.recv_slice(&mut loopback_data[..]).unwrap();
        }

        if socket.is_active() && loopback_data_length > 0 && socket.can_send() && loopback_data_length > 0 {
            socket.send_slice(&loopback_data[0..loopback_data_length]).unwrap();
            loopback_data_length = 0;
        }
    }
}
