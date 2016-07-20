<?php
  require_once("includes/common.php");
  nav_start_outer("Users");
  nav_start_inner();
?>
 <form name="profileform" method="GET"
  action="<?php echo $_SERVER['PHP_SELF']?>">
 <nobr>User:
 <input type="text" name="user" value="<?php
   // Beware: Stripping slashes is equivalent
   // to running PHP with magic_quotes_gpc off.
   echo stripslashes($_GET['user']);
 ?>" size=10>
 <input type="submit" value="View"></nobr>
</form>
<div id="profileheader"><!-- user data appears here --></div>
<?php
  $selecteduser = $_GET['user'];
  $sql = "SELECT Profile, Username, DikuCoins FROM Person " .
         "WHERE Username='$selecteduser'";
  $rs = $db->query($sql);
  if ( $row = $rs->fetch_assoc() ) { // Sanitize and display profile
      $profile = $row["Profile"];
      $usename = $row["Username"];
      $coins = intval($row["DikuCoins"]);

      echo "<div class=profilecontainer><b>Profile</b>";
      $allowed_tags =
          '<a><br><b><h1><h2><h3><h4><i><img><li><ol><p><strong><table>' .
          '<tr><td><th><u><ul><em><span>';
      $profile = strip_tags($profile, $allowed_tags);
      $disallowed =
          'javascript:|window|eval|setTimeout|setInterval|target|'.
          'onAbort|onBlur|onChange|onClick|onDblClick|'.
          'onDragDrop|onError|onFocus|onKeyDown|onKeyPress|'.
          'onKeyUp|onLoad|onMouseDown|onMouseMove|onMouseOut|'.
          'onMouseOver|onMouseUp|onMove|onReset|onResize|'.
          'onSelect|onSubmit|onUnload';
      $profile = preg_replace("/$disallowed/i", " ", $profile);
      echo "<p id=profile>$profile</p></div>";
  } else if($selecteduser) {  // user parameter present but user not found
      echo '<p class="warning" id="baduser">Cannot find that user.</p>';
  }
  $coins = ($coins > 0) ? $coins : 0;
  echo "<span id='coins' class='$coins'/>";
?><script type="text/javascript">
  var total = eval(document.getElementById('coins').className);
  function showCoins(coins) {
    document.getElementById("profileheader").innerHTML =
      "<?php echo $selecteduser ?>'s DIKU Coins: " + coins;
    if (coins < total) {
      setTimeout("showCoins(" + (coins + 1) + ")", 100);
    }
  }
  if (total > 0) showCoins(0);  // count up to total
</script>
<?php
  nav_end_inner();
  nav_end_outer();
?>
