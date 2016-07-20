require 'socket'
require 'net/http'

serv = TCPServer.new('localhost', 1339)

loop do
  Thread.start(serv.accept) do |s|
      req = s.gets
      puts req
      req = req.split
      r = req[0]
      case  r
        when "GET"
          if (req[1][-1] == "/")
            dir = "./jqapi" + req[1]
            if !File.exist?(dir)
              s.write "HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\n"
              s.puts "Page not found: 404"
              s.close
            end
            path = dir + "index.html"
            if File.exists?(path)
              file = File.new(path, "r")
              s.write "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n"
              while (line = file.gets)
                s.puts line
              end
              file.close
            else
              s.write "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n"
              Dir.entries(dir).each do |f|  
                if File.directory?(f) and !(f =='.' || f == '..')
                  s.puts("#{f}/")
                else 
                  if !(f =='.' || f == '..')
                    s.puts("#{f}")
                  end
                end
              end
            end
          else
            path = "./jqapi" + req[1]
            if File.exists?(path)
              file = File.new(path, "r")
              s.write "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n"
              while (line = file.gets)
                s.puts line
              end
              file.close
            else
              s.write "HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\n"
              s.puts "Page not found: 404"
            end
          end
        when "PROPFIND"
          s.write '<?xml version="1.0" encoding="utf-8"?>
                      <multistatus xmlns="DAV:">
                          <response>
                              <href>/</href>
                              <propstat>
                                  <prop>
                                      <resourcetype><collection/></resourcetype>
                                  </prop>
                                  <status>HTTP/1.1 200 OK</status>
                              </propstat>
                          </response>
                      </multistatus>'
        when "PUT"
          puts "Do put"
        when "DELETE"
          puts "Do delete"
        else
          puts r
          s.write "HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n"
          s.puts "Bad request: 400"
      end
    s.close
  end
end
