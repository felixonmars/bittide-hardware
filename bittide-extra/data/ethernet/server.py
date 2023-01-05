import socket
import threading
import time
import random

fpgaIP = "192.168.1.128"
fpgaPort = 1234

fpga_address = (fpgaIP, fpgaPort)
localIp = "192.168.1.127"
localPort = 20001

local_address = (localIp, localPort)


# Create a socket for sending and receiving UDP packets
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

sock.bind(local_address)
sock.settimeout(2)
packets = 100000

for packetsize in [16, 32, 64, 128, 256, 512, 1024]:
  sending = True
  # Thread for sending messages
  def send_thread():
    cnt = 1
    for i in range(packets):
      data = bytearray(random.getrandbits(8) for _ in range(packetsize))
      # Send a message
      sock.sendto(data, fpga_address)
      time.sleep(0.00001)
      cnt = cnt + 1

  # Thread for receiving messages
  def recv_thread():
    cnt = 1
    starttime = time.time()
    while sending:
      # Receive a message
      try:
        data, addr = sock.recvfrom(packetsize)
      except Exception:
        pass
      newTime = time.time()
      cnt = cnt + 1
    stoptime = time.time()
    print(packetsize, cnt / packets, (stoptime - starttime))



  # Start the sending and receiving threads
  send_thread = threading.Thread(target=send_thread)
  recv_thread = threading.Thread(target=recv_thread)
  recv_thread.start()
  send_thread.start()

  send_thread.join()
  sending = False
  recv_thread.join()
