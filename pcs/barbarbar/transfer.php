<?php
  require_once("includes/common.php");
  nav_start_outer("Transfer");
  nav_start_inner();
  if($_POST['submission']) {
    $recipient = $_POST['recipient'];
    $coins = (int) $_POST['coins'];
    $sql = "SELECT DikuCoins FROM Person WHERE PersonID=$user->id";
    $rs = $db->query($sql);
    $sender_balance = intval($rs->fetch_row()[0]) - $coins;
    $sql = "SELECT PersonID FROM Person WHERE Username='$recipient'";
    $rs = $db->query($sql);
    $recipient_exists = $rs->fetch_row()[0];
    if($coins > 0 && $sender_balance >= 0 && $recipient_exists) {
      $sql = "UPDATE Person SET DikuCoins = $sender_balance " .
             "WHERE PersonID=$user->id";
      $db->query($sql);
      $sql = "SELECT DikuCoins FROM Person WHERE Username='$recipient'";
      $rs = $db->query($sql);
      $recipient_balance = intval($rs->fetch_row()[0]) + $coins;
      $sql = "UPDATE Person SET DikuCoins = $recipient_balance " .
             "WHERE Username='$recipient'";
      $db->query($sql);
      $result = "Sent $coins DIKU Coins";
    }
    else $result = "Transfer to $recipient failed.";
  }
?>
<p><b>Balance:</b>
<span id="myCoins"></span> DIKU Coins</p>
<form method=POST name=transferform
  action="<?php echo $_SERVER['PHP_SELF']?>">
<p>Send <input name=coins type=text value="<?php
  echo $_POST['coins'];
?>" size=5> DIKU Coins</p>
<p>to <input name=recipient type=text value="<?php
  echo $_POST['recipient'];
?>"></p>
<input type=submit name=submission value="Send">
</form>
<span class=warning><?php
  echo $result;
?></span>
<?php
  nav_end_inner();
?>
<script type="text/javascript" src="barbarbar.js.php"></script>
<?php
  nav_end_outer();
?>
