<?
  // rfq695 - Nikolaj Dybdahl Rathcke
  
  try{
		//Connection to DB.
		$username = 'db_119';
		$password = 'db2013';

		$conn = new PDO('oci:dbname=//localhost:1521/dbwc', $username, $password);
		$conn->beginTransaction();
		$conn->exec("LOCK TABLE P_Products IN MODE EXCLUSIVE NOWAIT");

$statement = $conn->query('SELECT NVL(MAX(productcode), 0)+1 FROM P_Products');
$result = $statement->fetchAll();
$newid = $result[0][0];
$insert = $conn->prepare('INSERT INTO P_Products values(?,?,?,?,?,?,?,?,?)');

        //Info about insert.
        $na = $_REQUEST['name'];
        $pl = $_REQUEST['productline'];
        $sc = $_REQUEST['scale'];
        $ve = $_REQUEST['vendors'];
        $de = $_REQUEST['description'];
        $st = $_REQUEST['stock'];
        $pr = $_REQUEST['price'];
        $re = $_REQUEST['retailprice'];       

        $values = array($newid,$na,$pl,$sc,$ve,$de,$st,$pr,$re);
        
        $insert->execute($values);
	      $conn->commit();

	//Redirects back to the add_product page.
	header('Location: http://dbw.diku.dk/~rfq695/problem3_add_product.php');

	}
	catch(PDOException $e) {
	  print $e->getMessage();
		$conn->rollback();
	}

?>
