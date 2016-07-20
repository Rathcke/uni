#!/usr/bin/env python
from socket import AF_INET, SOCK_STREAM, socket
from random import randint
import os
import sys

def recvuntil(sock, stop):
    s = ''
    while True:
        c = sock.recv(1)
        if c == 0:
            raise Exception('Connection closed by server')
        if c == stop:
            return s
        s += c

def recvline(sock):
    return recvuntil(sock, '\n')

def sendline(sock, line):
    line = line + '\n'
    n = 0
    while n < len(line):
        n += sock.send(line[n:])

# Choose nick function
def setNick(sock, nick):
    sendline(sock, 'NICK %s' % nick)
    line = recvline(sock)
    if line.startswith('NICK OK'):
        print 'NICK, ok'
    else:
        print 'NICK, not ok:', line
        exit()

token = ''
# Join channel function
def joinChan(sock, chan):
    sendline(sock, 'JOINING #' + chan)
    line = recvline(sock)
    if line.startswith('TOKEN'):
        token = line.split(' ', 1)[1]
        print 'JOIN, ok'
    else:
        print 'JOIN, not ok:', line


# Start at length 1000 and init string s
j = 1000
s = ''

# Get line that crashes it if true
autowin = True

while not os.path.isfile('core'):

        # Get random random with length j
        s = os.urandom(j).replace('\x00', '\x41')
        s.replace('\x0a', '\x41')
        s.replace('\x20', '\x41')

        # If set to true, will get a line that works
        if autowin:
            f = open('ded2.txt', 'r')
            s = f.readline()
            f.close()

        # Try to connect to server, set nick and join channel
        try:

            # Connect to server
            sock = socket(AF_INET, SOCK_STREAM, 0)
            sock.connect(('localhost', 4242))
            sendline(sock, 'HELO SERVER')
            line = recvline(sock)

            # Nick
            setNick(sock, 'kringlekongen')

            # Join channel
            f = open('ded.txt', 'w')
            print >> f, s
            f.close()
            joinChan(sock, s)

        # Handle it if something goes wrong
        except:
            exception = sys.exc_info()[0].__name__
            if 'KeyboardInterrupt' in exception:
                break
        # Close socket
        sock.close()

        # Counter to keep track of length and increase it
        print j
        j += 1

# What turns out to be the address at where the segfault happens
for i in range(1005, 1009):
    print 'Index %d ' % i + '%d' % ord(s[i])
