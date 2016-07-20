<?php

require_once("includes/mydb.php");
require_once("login.php");
require_once("includes/auth.php");
require_once("includes/navigation.php");

// Init global variables
$db = mydb_connect();
$user = new User($db);

if(validate_user($user)) {
?>

    var myDikuCoins = <?php
          $sql = "SELECT DikuCoins FROM Person WHERE PersonID=$user->id";
          $rs = $db->query($sql);
          $balance = intval($rs->fetch_row()[0]);
          echo $balance > 0 ? $balance : 0;
        ?>;
    var div = document.getElementById("myCoins");

    if (div != null)
    {
        div.innerHTML = myDikuCoins;
    }
<?php
    }
?>
