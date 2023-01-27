import socket
import threading
import time
import random
import numpy

fpgaIP = "192.168.1.133"
fpgaPort = 1234

fpga_address = (fpgaIP, fpgaPort)
localIp = "192.168.1.127"
broadcastIp = "192.168.1.255"
broadcastAddress = (broadcastIp, fpgaPort)
localPort = 20001

local_address = (localIp, localPort)


# Create a socket for sending and receiving UDP packets
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
sock.bind(local_address)
sock.settimeout(2)
packets = 100000


def reliable_send(data, addr):
  noAck = True
  while noAck:
    sock.sendto(data, addr)
    try:
      data, addr = sock.recvfrom(1024)
      noAck = False
    except Exception:
      pass
  return (data, addr)
  # Thread for receiving messages

(_,fpgaAddress) = reliable_send(bytes(1), broadcastAddress)

def toWishbone(byteEnable : numpy.uint8, address : numpy.uint32, data : numpy.uint32):
  return bytearray([bytes(byteEnable), bytes(address), bytes(data)])

print("Found fpga at:", fpgaAddress)

reliable_send(bytearray((255,0,0,0,0,255,255,255,69)), fpgaAddress)
