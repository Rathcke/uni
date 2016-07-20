<! rfq695 - Nikolaj Dybdahl Rathcke >

<html>
<head>
	<title>
	View Order
	</title>
</head>
<h1>
	<center>View Order</center>
</h1>
<body>

<?php
	
	//connection to oracle DB
	try{

		$username = 'db_119';
		$password = 'db2013';
		
		$conn = new PDO('oci:dbname=//localhost:1521/dbwc', $username, $password);
		
		//sql code retrieving the ordernumber
    $oID = (isset($_REQUEST['ordernumber']) === true) ? 
            $_REQUEST['ordernumber'] : "";

    //sql code retrieving the information about the order.
    $ordersql = 'SELECT ordernumber, orderdate, requireddate, shippeddate, 
                 status, comments
                 FROM P_Orders
                 WHERE ordernumber =' . $oID;
		
		
		//Stores the returned rows in a variable.
		$resulting_rows_order = $conn->query($ordersql);
		
		//Printing the rows out.
    print '<table border="1px" style="margin:0 auto">';
  
    foreach ($resulting_rows_order as $row) {

   print '<center>';
    print '<tr><td><b>Number:</b></td><td> ' . $row[0] . '</td></tr>';
    print '<tr><td><b>Date:</b></td><td> ' . $row[1] . '</td></tr>';
    print '<tr><td><b>Required:</b></td><td> ' . $row[2] . '</td></tr>';
    print '<tr><td><b>Shipped:</b></td><td> ' . $row[3] . '</td></tr>';
    print '<tr><td><b>Status:</b></td><td> ' . $row[4] . '</td></tr>';
    print '<tr><td><b>Comments:</b></td><td> ' . $row[5] . '</td></tr>';
   print '</table></center>';
   
  }	
  
    //sql code retrieving the information about the products.
    $productsql = 'SELECT productname, quantityordered, priceeach
                   FROM P_Orderdetails NATURAL JOIN P_Products
                   WHERE ordernumber =' . $oID;
            
		//Stores the returned rows in a variable.
		$resulting_rows_products = $conn->query($productsql);
		
		//Printing the rows out.
    print '<table border="1px" style="margin:0 auto">';

    print '<td> Product </td><td> Quantity </td><td> Price </td>
           <td> Total </td></br>';

  foreach ($resulting_rows_products as $row) {

    print '<tr>';                                
     print '<td>' . $row[0] . '</td>
            <td>' . $row[1] . '</td>
            <td>' . $row[2] . '</td>
            <td>' . $row[1] * $row[2] . '</td>';           
     print '</tr>';
     $total = $total + $row[1] * $row[2];

  }
     print '<td> Total </td><td></td><td></td><td>' . $total . '</td>';
     print '</table>';
     
     print '<center>';
     print '</br></br><a href="problem2_show_orders.php"> Go back </a>';
     print '</center>';


	}
	catch(PDOException $e)
	{
		print "Error!: " . $e->getMessage() . "<br/>";
		die();
	}

?>

</body>

</html>
