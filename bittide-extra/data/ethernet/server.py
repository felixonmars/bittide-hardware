import socket
import threading
import time
import random

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

sending = True
  # Thread for receiving messages
def recv_thread():
  cnt = 1
  starttime = time.time()
  while sending:
    # Receive a message
    try:
      data, addr = sock.recvfrom(1024)
      print(data, addr)
    except Exception:
      pass
    newTime = time.time()
    cnt = cnt + 1

  # Start the sending and receiving threads
recv_thread = threading.Thread(target=recv_thread)
recv_thread.start()

data = bytearray(random.getrandbits(8) for _ in range(16))
while True:
  sock.sendto(data, broadcastAddress)
  time.sleep(1)

sending = False
recv_thread.join()
