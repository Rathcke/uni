require 'net/http'

uri = URI("localhost")

Net::HTTP.start("localhost", 1339) do |http|
  request = Net::HTTP::Get.new uri

  response = http.request request # Net::HTTPResponse object
end