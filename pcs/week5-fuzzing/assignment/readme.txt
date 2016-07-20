  .-------------------------------.
  |  PROACTIVE COMPUTER SECURITY  |
  |    Week 5: Fuzzing            |
  |      Weekly assignment        |
  '-------------------------------'

The program "yairc" is a chat server.  It is running on 163.172.148.91.  Your
task for this week is to create a fuzzer for the server which is able to crash
it.  Hand in the following:

1. A well commented fuzzer.  You can roll you own or use a framework,
   but in either case you must include directions on how to use your
   fuzzer.  We suggest that your fuzzer should be written in Python or
   C (check with your TA if you prefer something else).  Feel free to use
   fuzzer.py as a template.

2. A core file from the crashed server.

3. We have included an AFL instrumented binary (yairc-afl) which you may,
   or may not, find usful. Feel free to ignore it.


Protocol:
  The protocol is a line based TCP protocol.  The server listens on port 4242.
  A line must end on a single line feed character ('\n', hex 0xa).

  Handshake:
    A chat session starts with a handshake.  The client initiates a session by
    sending the line
      HELO SERVER
    and the server answers with
      HELO CLIENT

    Then the client chooses a username by sending
      NICK <username>
    where <username> is the desired username (excluding the <>).  The server
    answers with
      NICK OK (<username>)
    if the username is accepted.  Else it answers
      WRONG
    and closes the connection.

    To join a channel the client sends
      JOINING #<channel>
    where <channel> is the name of the channel (excluding the <>, but including
    the #).  If the channel name is accepted the server answers
      TOKEN <token>
    where <token> is a token associating the user and the channel.  This token
    must be remembered and used with some of the following commands.  If the
    channel name is not accepted the server will answer
      NOPE
    and close the connection.

  Commands:
    The client can send a number of commands, each one line.  The commands are at least:
      QUIT PLEASE
        Close the connection to the server.  The server answers
          GOODBEY
      WHO AM I
        Get current username.  The server answers with
          Y U NO REMEMBER? <username>
      TOPIC <token> <topic>
        Set the topic of the channel identified by <token> to <topic>.  The
        server does not answer this command.
      MSG <token> <message>
        Send message <message> to channel identified by <token>.  The server
        does not answer this command.
      LIST <token>
        List users in channel identified by <token>.  The server answers with
        one line per user of the form
          USER <username>
        for each user, followed by the line
          LIST END
      PART <token>
        Leave the channel identified by token <token>.  This invalidates the
        token.  The server answers with
          LEFT OK (<token>)
      NICK <username>
        Change username to <username>.  The server answers as during the
        handshake.

  Messages and topic change:
    At any time the server may send one of several messages:
      MSG <token> <time> <username> <message>
        Meaning that user <username> sent message <message> to the channel
        identified by token <token> at time <time>.  The format of <time> is
          YYYY-MM-DD HH:MM:SS
      TOPIC <token> <time> <username> <topic>
        Meaning that the topic of the channel identified by <token> has been
        changed to <topic> by <username> at <time>.
      JOIN <token> <time> <username>
        User <username> joined the channel identified by <token>.
      PART <token> <time> <username>
        User <username> parted the channel identified by <token>.


Hints:

1. Enable core dumps.  Write this in the shell where you will run the server:
     ulimit -c unlimited

2. Start the server:
     ./yairc

   If you get the error
     import error: No module named sysconfigdata_nd
   then you need the following fix
     apt-get install libpython2.7:i386

3. Open a new shell and try to connect to the server:
     nc localhost 4242
   Write 'HELO SERVER' and hit return.  You should see the server answering
   'HELO CLIENT'.

4. A simple command line client is included in client.py.  Run it by typing
     ./client.py
   Try to connect to 163.172.148.91.

5. When you manage to crash the server you can inspect the circumstances with
   gdb (core dumps must have been enabled, i.e. the file 'core' must exist):
     gdb yairc core

6. It is possible to gain arbitrary code execution on the official
   yairc server.  Feel free to used the included shellcodes
   bindshell.asm and connect-back.asm.

7. It is possible to start the server in local mode by giving the argument -local:
     ./yairc -local
    
   In local mode the server uses stdin and stdout instead of
   sockets. You may, or may not, find this easier to fuzz. But be
   aware that we ask you to find bug in the networked server, so if
   find a bug in local mode make sure to check that is also is in the
   networked version.

8. The server actually accept more commands that those documented
   above. You are given extra credit if you can the undocumented
   commands. Remember to specify how you found them.
