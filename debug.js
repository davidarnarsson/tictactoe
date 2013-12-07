var net = require('net');

var client = net.connect(2222, "localhost", function() {
  console.log('Client connected!')

  
});

client.on('data', function(data) {
  console.log(data.toString())

  client.write("hello yourself!\r\n", function() {
    console.log("wrote back!");
  });
});

client.on('end', function() {
  console.log('Client disconnected.')
});
