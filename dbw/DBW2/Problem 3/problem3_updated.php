<?
  // rfq695 - Nikolaj Dybdahl Rathcke
  
  try{
		//Connection to DB.
		$username = 'db_119';
		$password = 'db2013';

		$conn = new PDO('oci:dbname=//localhost:1521/dbwc', $username, $password);
		$conn->beginTransaction();
		$conn->exec("LOCK TABLE P_Products IN MODE EXCLUSIVE NOWAIT");
		
		//Info about update.
		$id = $_REQUEST['id'];
		$st = $_REQUEST['stock'];
		$pr = $_REQUEST['price'];
		$re = $_REQUEST['msrp'];

    $update = $conn->prepare('UPDATE P_Products
                              SET quantityinstock=' . $st . ', 
                              buyprice=' . $pr . ', MSRP=' . $re . '
                              WHERE productcode=' . $id);

    $update->execute();
	  $conn->commit();

	//Redirects back to the add_product page.
	header('Location: http://dbw.diku.dk/~rfq695/problem3_update_product.php');

		}
	catch(PDOException $e) {
	  print $e->getMessage();
		$conn->rollback();
	}

?>
