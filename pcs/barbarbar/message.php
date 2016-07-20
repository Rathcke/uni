<?php
  require_once("includes/common.php");
  nav_start_outer("Messages");
  nav_start_inner();
  $id = $_GET["id"];
  $sql = "SELECT Headline, Message FROM Message WHERE MessageID='$id'";
  $res = $db->query($sql);
  $row = $res->fetch_assoc();
?>
    <p><?php echo $row["Headline"]; ?></p>
    <p><?php echo $row["Message"]; ?></p>
<?php
  nav_end_inner();
  nav_end_outer();
?>
