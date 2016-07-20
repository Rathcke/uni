<! rfq695 - Nikolaj Dybdahl Rathcke >

<?php

  try{
		//Connection to DB.
		$username = 'db_119';
		$password = 'db2013';

		$conn = new PDO('oci:dbname=//localhost:1521/dbwc', $username, $password);

//Functions to retrieve vendors and productlines.		
function getVendors() {
	  global $conn;
 	  $getVendor = $conn->prepare('Select DISTINCT productvendor
 	                               FROM P_Products
 	                               ORDER BY 1');
	  $getVendor->execute();
	return $getVendor->fetchAll();	
}

function getProductlines() {
	  global $conn;
 	  $getProductlines = $conn->prepare('Select DISTINCT productline
 	                                     FROM P_Products
 	                                     ORDER BY 1');
	  $getProductlines->execute();
	return $getProductlines->fetchAll();
}

?>

<html>
<head>
	<title>
	Update Product
	</title>
</head>
<h1>
	<center>Update Product</center>
</h1>

<body>
<center>

<! Form to add new products>

<table> 
<form action="problem3_updated.php" method="POST">

<? 

//A form where only 3 of the fields are not readonly.		
if ($_REQUEST['code']) {
  
  $pID = $_REQUEST['code'];
          
  $name = $conn -> prepare('SELECT productname, productscale, productvendor, 
                                   productdescription, quantityinstock, 
                                   buyprice, msrp, productline
                            FROM P_Products
                            WHERE productcode =' . $pID);
  $name->execute();
  $attributes = $name->fetch(); 
          
  print '<tr><td>ID: </td><td><input type="text" name="id"
         value="' . $pID . '" readonly></td></tr>
         <tr><td>Name: </td><td><input type="text" 
         value="' . $attributes[0] . '" readonly></td></tr>
         <tr><td>Scale: </td><td><input type="text"
         value="' . $attributes[1] . '" readonly></td></tr>
         <tr><td>Vendor: </td><td><input type="text" 
         value="' . $attributes[2] . '" readonly></td></tr>    
         <tr><td>Description: </td><td><input type="text" 
         value="' . $attributes[3] . '" readonly></td></tr>
         <tr><td>Stock: </td><td><input type="text" name="stock" 
         value="' . $attributes[4] . '"></td></tr>
         <tr><td>Price: </td><td><input type="text" name="price" 
         value="' . $attributes[5] . '"></td></tr>
         <tr><td>Recommended retail price: </td><td><input type="text" 
         name="msrp" value="' . $attributes[6] . '"></td></tr>
         <tr><td>Vendor: </td><td><input type="text"
         value="' . $attributes[7] . '" readonly></td></tr>

         </table> </br>
         <input type="submit" value="Update Product"></br></br>
         </form>';

}

	  //sql to retrieve information about products.
	  $sql = 'SELECT productcode, productname, productline, productscale, 
	                 productvendor, quantityinstock, buyprice, msrp
	          FROM P_Products
	          ORDER BY productcode DESC';

		$resulting_rows = $conn->query($sql);

	  //Printing the rows out.
    print '<table border="1px" style="margin:0 auto">';
    
    print '<td> Code </td><td> Name </td><td> Line </td><td> Scale </td>
           <td> Vendor </td><td> Stock </td><td> Price </td><td> MSRP </td>';
          
  foreach ($resulting_rows as $row) {

    print '<tr>';                                
     print '<td>'. '<a href="problem3_update_product.php?code='. $row[0]. 
           '">' . $row[0] . '</a>' . '</td>
            <td>' . $row[1] . '</td>
            <td>' . $row[2] . '</td>
            <td>' . $row[3] . '</td>
            <td>' . $row[4] . '</td>
            <td>' . $row[5] . '</td>
            <td>' . $row[6] . '</td>
            <td>' . $row[7] . '</td>';
     print '</tr>';

  }
        print '</table>';

	}
	catch(PDOException $e)
	{
		print "Error!: " . $e->getMessage() . "<br/>";
		die();
	}

?>

</body>

</html>
