var page = require("webpage").create();
var fs = require("fs");
var url = "http://localhost/messages.php";
var file = "latest";
var latestMessage = "";
if (fs.exists(file)) {
    latestMessage = fs.read(file);
}
page.onConsoleMessage = function(msg) {
  console.log(msg);
};
page.open(url, function() {
    page.includeJs("http://code.jquery.com/jquery-1.12.3.min.js", function() {
        page.evaluate(function() {
            $("input[name='login_username']").val("rloewe");
            $("input[name='login_password']").val("herpderp");
            $("input[name='submit_login']").click();
        });
        page.onLoadFinished = function() {
            var newpage = require("webpage").create();
            newpage.open(url, function() {
                var a = newpage.evaluate(function() {
                    var as = document.querySelectorAll(".messages a");
                    if (as.length > 0) {
                        return as[as.length-1];
                    } else {
                        return undefined;
                    }
                });
                if (a) {
                    var message = a.href;
                    if (latestMessage !== message) {
                        fs.write(file, message, 'w');
                        var messagepage = require("webpage").create();
                        messagepage.onConsoleMessage = function(msg) {
                            console.log(msg);
                        };
                        messagepage.onCallback = function() {
                            phantom.exit();
                        };
                        messagepage.open(message, function() {
                            messagepage.evaluate(function() {
                                var readyStateCheckInterval = setInterval(function() {
                                    if (document.readyState === "complete") {
                                        clearInterval(readyStateCheckInterval);
                                        window.callPhantom("done");
                                    }
                                }, 100);
                            });
                        });
                    } else {
                        phantom.exit();
                    }
                } else {
                    phantom.exit();
                }
            });
        };
    });
});
