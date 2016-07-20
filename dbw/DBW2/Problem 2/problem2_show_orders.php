<! rfq695 - Nikolaj Dybdahl Rathcke >

<html>
<head>
	<title>
	Orders
	</title>
</head>
<h1>
	<center>Orders</center>
</h1>
<body>

<?php

  //connection to oracle DB.
  try{
	  $username = 'DB_119';
	  $password = 'db2013';

	  $conn = new PDO('oci:dbname=//localhost:1521/dbwc', $username, $password);

print '<table border="1px" style="margin:0 auto">';	
print '<td><a href="problem2_show_orders.php?sort=number"> Number </a></td>
       <td><a href="problem2_show_orders.php?sort=date"> Date </a></td>
       <td><a href="problem2_show_orders.php?sort=status"> Status </a></td>
       <td><a href="problem2_show_orders.php?sort=customer"> Customer </a></td>
       <td><a href="problem2_show_orders.php?sort=employee"> Employee </a></td>
       <td><a href="problem2_show_orders.php?sort=office"> Office </a></td>';

	  //SQL statement to retrieve the info about orders.
    $sort = 'ordernumber';

    if ($_GET['sort'] == 'number') {
    $sort = 'ordernumber';
    }
    if ($_GET['sort'] == 'date') {
    $sort = 'orderdate';
    }
    if ($_GET['sort'] == 'status') {
    $sort = 'status';
    }
    if ($_GET['sort'] == 'customer') {
    $sort = 'customername';
    }
    if ($_GET['sort'] == 'employee') {
    $sort = 'lastname';
    }
    if ($_GET['sort'] == 'office') {
    $sort = 'officecode';
    }

    $infosql = 'SELECT ordernumber, orderdate, status, customername, lastname,
	                     firstname, officecode, customernumber
			          FROM ((P_Orders NATURAL JOIN P_Customers) JOIN P_Employees 
			               ON salesrepemployeenumber = employeenumber) NATURAL JOIN 
			               P_Offices
			          ORDER BY ' . $sort;

    $resulting_rows = $conn->query($infosql);

		//Printing the rows out.	
  foreach ($resulting_rows as $row) {

    print '<tr>';                                
     print '<td>' . '<a href="problem2_view_order.php?ordernumber=' . $row[0] . 
           '">' . $row[0] . '</a>' . '</td>
           <td>' . $row[1] . '</td>
           <td>' . $row[2] . '</td>
           <td>' . '<a href="problem2_view_customer.php?customernumber=' . 
           $row[7] . '">'. $row[3] . '</a>' .  '</td>
           <td>' . $row[4] . ', ' . $row[5]. '</td>
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
