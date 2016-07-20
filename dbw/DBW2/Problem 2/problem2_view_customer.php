<! rfq695 - Nikolaj Dybdahl Rathcke >

<html>
<head>
	<title>
	View Customer
	</title>
</head>
<h1>
	<center>View Customer</center>
</h1>
<body>

<?php

	//connection to oracle DB
	try{

		$username = 'db_119';
		$password = 'db2013';

		$conn = new PDO('oci:dbname=//localhost:1521/dbwc', $username, $password);

		//sql code retrieving the customernumber
    $cID = (isset($_REQUEST['customernumber']) === true) ? 
            $_REQUEST['customernumber'] : "";

    //sql code retrieving the information about the customer.
    $customersql = 'SELECT customernumber, customername, phone, addressline1, 
                    addressline2, city, state, postalcode, country, creditlimit
                    FROM P_Customers
                    WHERE customernumber =' . $cID;

		//Stores the returned rows in a variable.
		$resulting_rows_costumer = $conn->query($customersql);

		//Printing the rows out.
    print '<table border="1px" style="margin:0 auto">';

  foreach ($resulting_rows_costumer as $row) {

   print '<center>';
    print '<tr><td><b>Number:</b></td><td> ' . $row[0] . '</td></tr>';
    print '<tr><td><b>Name:</b></td><td> ' . $row[1] . '</td></tr>';
    print '<tr><td><b>Phone:</b></td><td> ' . $row[2] . '</td></tr>';
    print '<tr><td><b>Adress1:</b></td><td> ' . $row[3] . '</td></tr>';
    print '<tr><td><b>Adress2:</b></td><td> ' . $row[4] . '</td></tr>';
    print '<tr><td><b>City:</b></td><td> ' . $row[5] . '</td></tr>';
    print '<tr><td><b>State:</b></td><td> ' . $row[6] . '</td></tr>';
    print '<tr><td><b>Postalcode:</b></td><td> ' . $row[7] . '</td></tr>';
    print '<tr><td><b>Country:</b></td><td> ' . $row[8] . '</td></tr>';
    print '<tr><td><b>Credit:</b></td><td> ' . $row[9] . '</td></tr>';
   print '</table></center></br>';
   
  }
  
  //sql code retrieving the information about the payment.
    $paymentsql = 'SELECT checknumber, paymentdate, amount
                   FROM P_Payments
                   WHERE customernumber =' . $cID;
            
		//Stores the returned rows in a variable.
		$resulting_rows_payment = $conn->query($paymentsql);

		//Printing the rows out.
		print '<center><h2> Payments </h2></center>';
    print '<table border="1px" style="margin:0 auto">';
    
    print '<td> Checknumber </td><td> Date </td><td> Amount </td>';
          
    foreach ($resulting_rows_payment as $row) {

    print '<tr>';                                
     print '<td>' . $row[0] . '</td>
            <td>' . $row[1] . '</td>
            <td>' . $row[2] . '</td>';            
    print '</tr>';
    $total = $total + $row[2];

  }
     print '<td> Total </td><td></td><td>' . $total . '</td>';
     print '</table>';

     //sql code retrieving the total price for all order of a customer.
     $totalsql = 'SELECT quantityordered, priceeach
                  FROM P_Orders NATURAL JOIN P_Orderdetails
                  WHERE customernumber =' . $cID; 

     $resulting_rows_total = $conn->query($totalsql);

     foreach ($resulting_rows_total as $row) {

     $ordertotal = $ordertotal + $row[0] * $row[1];

    }

     //balance is printed out.
     $balance = $total - $ordertotal;
     print '</br><center><b> Balance: </b>' . $balance . '</center>';

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
