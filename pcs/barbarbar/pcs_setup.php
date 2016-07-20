<?php
require_once("includes/mydb.php");

$db = mydb_connect();

$sql = "INSERT INTO Person (Username, Password, Salt, DikuCoins, Profile) VALUES ".
     "('kflarsen', '29006d99bd6d477664f43ee3a5dde925', 'salt', 42, 'Bad Cop'), " .
     "('br0ns', '54822fa5a4ea6ca8fea0ade431756664', 'beef', 42, 'Bad Cop'), " .
     "('kokjo', 'fe7ecc4de28b2c83c016b5c6c2acd826', '4242', 42, 'Bad Cop'), " .
     "('kristoff3r', 'fe7ecc4de28b2c83c016b5c6c2acd826', '4242', 42, 'Bad Cop'), " .
     "('NicolaiNebel', 'fe7ecc4de28b2c83c016b5c6c2acd826', '4242', 42, 'Bad Cop'), " .
     "('rloewe', '7bfd638c108b8dba714aeb2e8349603c', '4242', 42, 'Bad Cop'), " .
     "('you', '5a8441c3702aacd4103fd8675692f015', 'evil', 10, 'So bad')"
    ;

$rs = $db->query($sql);
header("Location: index.php");
exit();
?>
