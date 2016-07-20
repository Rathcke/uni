<?php
  require_once("includes/common.php");
  nav_start_outer("Home");
  nav_start_inner();
?>
<b>Balance:</b>
<?php
  $sql = "SELECT DikuCoins FROM Person WHERE PersonID=$user->id";
  $rs = $db->query($sql);
  $balance = intval($rs->fetch_row()[0]);
  echo $balance > 0 ? $balance : 0;
?> DIKU Coins<br/>
<b>Your profile:</b>
<form method="POST" name=profileform
  action="<?php echo $_SERVER['PHP_SELF']?>">
<textarea name="profile_update">
<?php
  if($_POST['profile_submit']) {  // Check for profile submission
    $profile = $_POST['profile_update'];
    $sql = "UPDATE Person SET Profile='$profile' ".
           "WHERE PersonID=$user->id";
    $db->query($sql);  // Overwrite profile in database
  }
  $sql = "SELECT Profile FROM Person WHERE PersonID=$user->id";
  $rs = $db->query($sql);
  echo $rs->fetch_row()[0];  // Output the current profile
?>
</textarea><br/>
<input type=submit name="profile_submit" value="Save"></form>
<?php
  nav_end_inner();
  nav_end_outer();
?>
