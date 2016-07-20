require 'socket'

serv = TCPServer.new('localhost', 1339)

loop do
  Thread.start(serv.accept) do |s|
      req = s.gets
      req = req.split
      if (req[0] == "GET")
        if (req[1][-1] == "/")
          dir = "." + req[1]
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
          path = "." + req[1]
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
      else
        s.write "HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n"
        s.puts "Bad request: 400"
      end
    s.close
  end
end
