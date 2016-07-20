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
	Add Product
	</title>
</head>
<h1>
	<center>Add Product</center>
</h1>

<body>
<center>

<! Form to add new products>

<table> 
<form action="problem3_added.php" method="POST">
 <tr><td>Name: </td><td><input type="text" name="name"> </td></tr>
 <tr><td>Scale: </td><td><input type="text" name="scale"> </tr></td>
 <tr><td>Vendor: </td><td><select name="vendors">
         <option value="0"> Select Vendor </option>      
<?

  foreach(getVendors() as $rows) {
    print '<option value="' . $rows[0] . '">' . $rows[0] . '</option>';
  } 
  
?>
  </select></td></tr>

 <tr><td>Description: </td><td><input type="text" name="description"> </td></tr>
 <tr><td>Stock: </td><td><input type="text" name="stock"> </td></tr>
 <tr><td>Price: </td><td><input type="text" name="price"> </td></tr>
 <tr><td>Recommended retail price:</td><td><input type="text"
                                                  name="retailprice"> </td></tr>
 <tr><td>Productline: </td><td><select name="productline"> 
                      <option value="0"> Select Productline </option>

<?

  foreach(getProductlines() as $rows) {
    print '<option value="' . $rows[0] . '">' . $rows[0] . '</option>';
  } 
  
?>
  </select> </td></tr>
  </table> </br>
<input type="submit" value="Add Product">
</form>
</center></br>

<?
	  //sql to retrieve information about products.
	  $sql = 'SELECT productname, productline, productscale, productvendor, 
	                 quantityinstock, buyprice, msrp
	          FROM P_Products
	          ORDER BY productcode DESC';

		$resulting_rows = $conn->query($sql);

	  //Printing the rows out.
    print '<table border="1px" style="margin:0 auto">';
    
    print '<td> Name </td><td> Line </td><td> Scale </td><td> Vendor </td>
           <td> Stock </td><td> Price </td><td> MSRP </td>';
          
  foreach ($resulting_rows as $row) {

    print '<tr>';                                
     print '<td>' . $row[0] . '</td>
            <td>' . $row[1] . '</td>
            <td>' . $row[2] . '</td>
            <td>' . $row[3] . '</td>
            <td>' . $row[4] . '</td>
            <td>' . $row[5] . '</td>
            <td>' . $row[6] . '</td>';
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
