<?php

// Cookie-based authentication logic

class User {
  var $db = null;
  var $id = 0; // the current user's id
  var $username = null;
  var $cookieName = "BarbarbarLogin";
  function User(&$db) {
    $this->db = $db;
    if ( isset($_COOKIE[$this->cookieName]) ) {
      $this->_checkRemembered($_COOKIE[$this->cookieName]);
    }
  }

  function _checkLogin($username, $password) {
    $sql = "SELECT Salt FROM Person WHERE Username = '$username'";
    $rs = $this->db->query($sql);
    $salt = $rs->fetch_row()[0];
    $hashedpassword = md5($password.$salt);
    $sql = "SELECT * FROM Person WHERE " .
           "Username = '$username' AND " .
           "Password = '$hashedpassword'";
    $result = $this->db->query($sql);
    if ( $values = $result->fetch_assoc() ) {
      $this->_setCookie($values, true);
      return true;
    } else {
      return false;
    }
  }

  function _addRegistration($username, $password) {
    $sql = "SELECT PersonID FROM Person WHERE Username='$username'";
    $rs = $this->db->query($sql);
    if( $rs->num_rows > 0 ) return false;  // User already exists
    $salt = substr(md5(rand()), 0, 4);
    $hashedpassword = md5($password.$salt);
    $sql = "INSERT INTO Person (Username, Password, Salt, DikuCoins) " .
           "VALUES ('$username', '$hashedpassword', '$salt', 10)";
    $this->db->query($sql);
    return $this->_checkLogin($username, $password);
  }

  function _logout() {
    if(isset($_COOKIE[$this->cookieName])) setcookie($this->cookieName);
    $this->id = 0;
    $this->username = null;
  }

  function _setCookie(&$values, $init) {
    $this->id = $values["PersonID"];
    $this->username = $values["Username"];
    $token = md5($values["Password"].mt_rand());
    $this->_updateToken($token);
    $session = session_id();
    $sql = "UPDATE Person SET Token = '$token' " .
           "WHERE PersonID = $this->id";
    $this->db->query($sql);
  }

  function _updateToken($token) {
    $arr = array($this->username, $token);
    $cookieData = base64_encode(serialize($arr));
    setcookie($this->cookieName, $cookieData, time() + 31104000);
  }

  function _checkRemembered($cookie) {
    $arr = unserialize(base64_decode($cookie));
    list($username, $token) = $arr;
    if (!$username or !$token) {
      return;
    }
    $sql = "SELECT * FROM Person WHERE " .
           "(Username = '$username') AND (Token = '$token')";
    $rs = $this->db->query($sql);
    if ( $row = $rs->fetch_assoc() ) {
      $this->id = $row["PersonID"];
      $this->username = $row["Username"];
    }
  }
}
?>
