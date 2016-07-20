Barbar Bar
==========


Background
----------

The fictional "Barbar Bar Foundation" has started to develop a simple
web application, barbarbar, allowing registered users to post profiles and
transfer "DIKU Coin" credits between each other. Each registered user
starts with 10 DIKU Coins.

You will craft a series of attacks on the web application that exploit
vulnerabilities in the website's design. Each attack presents a
distinct scenario with unique goals and constraints, although in some
cases you may be able to re-use parts of your code.

Although many real-world attackers do not have the source code for the
web sites they are attacking, you are one of the lucky ones: source
code is available. You won't actually need to look at the site's
source code until White Box testing in Part 2, but it's there if you
get stuck.


Setup
-----

We will grade your project with the default settings using the release
of the Iceweasel (Firefox) browser included at the VM. We chose this
browser for grading because it is widely available and can run on a
variety of operating systems. There are subtle quirks in the way HTML
and JavaScript are handled by different browsers, and some attacks
that work in Internet Explorer (for example) may not work in
Iceweasel/Firefox. We recommend that you test your code on
Iceweasel/Firefox before you submit, to ensure that you will receive
credit for your work.

Inside the VM the barbarbar web app can be accessed at:
<http://localhost/barbarbar> the following users are already
registered: kflarsen, br0ns, kokjo, kristoff3r, NicolaiNebel, rloewe
and you. The password for you is mastermind.

The source code for barbarbar is available in the local file system
at: `/var/www/html/barbarbar` if/when you want to view it.


What to Hand In
---------------

You should hand in two things:
  - A report (as a text file or PDF file) with findings.

    Your report should be no longer than four pages (excluding
    appendices and code examples) and should be composed for ease of
    grading.  That is, structure your report so that the first four
    pages makes it clear why you should pass the assignment, and
    include minor finding in appendices.

  - Source code you may have produced as part of your hand-in, in
    a zip archive.

The Appendix B contains an example of how we would like you to structure your
hand-in.

Part 1: Black Box Assessment
----------------------------

Even though you have access to the source code of barbarbar, you
should (try to) complete this part of the assignment as a *Black Box*
assessment.  That is, without looking at the source code for
barbarbar.  And you should only access barbarbar through the web
interface.

## Attack A. Cookie Theft

Your solution is a message sent to the user rloewe (he checks his messages once
in a while, even on your machine).

 * Your goal is to steal the document cookie and send it back to yourself.

 * The page the grader sees when checking the message should look exactly as it
   normally does, when the grader visits message.php. No changes to the site
   appearance or extraneous text should be visible. Avoiding the red warning
   text is an important part of this attack. (It's ok if the page looks
   weird briefly before correcting itself.)

 * Hint: `<script>alert(document.cookie);</script>` is an example attack to
   use as a starting point.


## Attack B. Cross-Site Request Forgery

* Your solution is a short HTML document that the grader will open
  using the web browser.

* The grader will already be logged in (not as you) to the local
  barbarbar before loading your page.

* Transfer 10 DIKU Coins from the grader's account to the 'you'
  account. As soon as the transfer is complete the browser should be
  redirected to <http://yesimparanoid.com/> (so fast the user
  might not notice).

* The location bar of the browser should not contain localhost at any
  point.


## Extra Fun (Optional)

### Profile Worm:

* Your solution is a profile that, when viewed, transfers 1 DIKU Coin
  from the current user to the user called 'you' and replaces the
  profile of the current user with itself.

* Your malicious profile may include a witty message to the grader
  (optional, but it helps us see that it replicated).

* To grade your attack, we will cut and paste the submitted profile
  file into the profile of the 'you' user and view that profile
  using the grader's account. We will then view the copied profile
  with more accounts, checking for the transfer and replication.

* The transfer and replication should be reasonably fast (under 5
  seconds). During that time, the grader will not click anywhere.

* During the transfer and replication process, the browser's location
  bar should remain at
  <http://localhost/barbarbar/users.php?user=username>, where
  `username` is the user whose profile is being viewed. The visitor
  should not see any extra graphical user interface elements (e.g.,
  frames), and the user whose profile is being viewed should appear to
  have 10 DIKU Coins.

* Hint: The site allows a sanitized subset of HTML in profiles, but
  you can get around it. This MySpace vulnerability may provide some
  inspiration: <https://en.wikipedia.org/wiki/Samy_%28XSS%29>


### Password Theft:

* Your solution is a short HTML document that the grader will open
  using the web browser.

* The grader will not be logged in to the local barbarbar before
  loading your page.

* Upon loading your document, the browser should immediately be
  redirected to <http://localhost/barbarbar/>. The grader will enter a
  username and password and press the "Log in" button.

* When the "Log in" button is pressed, send the username and password
  (separated by a comma) to and optional URL of your choice.

* The login form should appear perfectly normal to the user. No
  extraneous text (e.g. warnings) should be visible, and assuming the
  username and password are correct the login should proceed the same
  way it always does.

* Hint: The site uses `htmlspecialchars` to sanitize the reflected
  username, but something is not quite right.


Part 2: White Box Assessment
----------------------------

In this task you are allowed (but not required) to look at the
source code for barbarbar.  If you solve the task as a black box
assessment, let us know and we'll give you extra credit.

You shall focus on SQL Injection.  Your main tasks are to:
  - See if you can get a list over registered users and their
    information (in particular their password).
  - See if you can change the password for 'br0ns' and be able to login with
    his new credentials.


Part 3: Fix It
--------------

Select an SQL injection issue you have found, and fix that and Attack
A and B.  That is, find (some of) the relevant places were HTML or SQL
can be inputted by a user, and implement sanitizers for the input.

Demonstrate that your have fixed the issues by writing tests that
demonstrates that the issues have been fixed. Your tests should be
more than just string comparisons of bad input to good output, as it
should not depend on specific implementation of the sanitizers. You
can assume that non-dangerous HTML is left alone and that dangerous
HTML is removed or replaced, but should not depend on the specific way
that the transformation is performed.


