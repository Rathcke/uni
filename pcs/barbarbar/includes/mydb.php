<?php

function initialise_barbarbar(&$db) {
    if (! $db->query(
        'CREATE TABLE IF NOT EXISTS Person (
            PersonID  INT PRIMARY KEY AUTO_INCREMENT,
            Password  TEXT NOT NULL,
            Salt      CHAR(4) NOT NULL,
            Username  TEXT NOT NULL,
            DikuCoins INT NOT NULL,
            Profile   TEXT,
            Token     CHAR(32))') ) {
        die ("Table creation failed: (" . $db->errno . ") " . $db->error);
    }
    if (!$db->query("CREATE TABLE IF NOT EXISTS Message (
            MessageID INT PRIMARY KEY AUTO_INCREMENT,
            FromID INT NOT NULL,
            ToID INT NOT NULL,
            Headline TEXT NOT NULL,
            Message TEXT NOT NULL
        )")) {
        die ("Table creation failed: (" . $db->errno . ") " . $db->error);
    }
}

// function for establishing connection to the database
function mydb_connect() {
    $dbhost = "localhost";
    $user = "barman";
    $passwd = "secret";
    $database = "barbarbar";

    $db = mysqli_connect($dbhost, $user, $passwd, $database);
    if ($db->connect_errno) {
        die ("Failed to connect to MySQL: (" . $db->connect_errno . ") " . $db->connect_error);
    }
    initialise_barbarbar($db);
    return $db;
}
?>
