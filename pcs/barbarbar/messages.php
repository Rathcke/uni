<?php
  require_once("includes/common.php");
  nav_start_outer("Messages");
  nav_start_inner();

  if ($_POST["message_submit"]) {
      $sql = "SELECT PersonID FROM Person WHERE Username = '" . $_POST["user"] . "'";
      $rs = $db->query($sql);
      if ($row = $rs->fetch_assoc()) {
          $from = $user->id;
          $to = $row["PersonID"];
          $headline = $_POST["headline"];
          $message = $_POST["message"];
          $sql = "INSERT INTO Message (FromID, ToID, Headline, Message) " .
                 " VALUES ('$from', '$to', '$headline', '$message')";
          if (!$db->query($sql)) {
              die("Making message failed: (" . $db->errno . ") " . $db->error);
          }
      }
  }
?>
 <form name="profileform" method="POST"
  action="<?php echo $_SERVER['PHP_SELF']?>">
 To:
 <input type="text" name="user" value="" size=10>
<br/>
 Headline:
 <input type="text" name="headline" size=10>
 <textarea name="message"></textarea>
 <input type="submit" name="message_submit" value="Send"></nobr>
</form>
<?php
  $sql = "SELECT m.MessageID, m.Headline, p.Username FROM Message as m " .
         "JOIN Person as p ON p.PersonID = m.FromID" .
         " WHERE ToID='$user->id'";
  $rs = $db->query($sql);
?>
<table class="messages">
    <tr>
        <td>From</td>
        <td>Headline</td>
    </tr>
<?php
  while ( $row = $rs->fetch_assoc() ) { // Sanitize and display profile
      $ID = $row["MessageID"];
      $from = $row["Username"];
      $headline = $row["Headline"];

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
      $headline = preg_replace("/$disallowed/i", " ", $headline);
      echo "<tr><td>$from</td><td><a href='message.php?id=$ID'>". substr($headline, 0, 25) . "</a></td></tr>";
  }
  $coins = ($coins > 0) ? $coins : 0;
?>
</table>
<?php
  nav_end_inner();
  nav_end_outer();
?>
