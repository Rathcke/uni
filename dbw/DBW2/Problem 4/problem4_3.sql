create or replace 
FUNCTION ORDERPRINT (ORDERID NUMBER) 
RETURN varchar2 IS res VARCHAR(4000) :='"';

FIRST INTEGER := 1;
oDate date;
oStatus VARCHAR2(15);

BEGIN
  SELECT orderdate INTO oDate
  FROM P_ORDERS
  WHERE ordernumber = orderid;
  res:= 'date:' || to_char(oDate, 'YYYY-MM-DD') || '; ';
  SELECT status INTO oStatus
  FROM P_ORDERS
  WHERE ordernumber = orderid;
  res := res || 'status:' || oStatus || '; ';
  FOR p IN (SELECT productname
            FROM P_ORDERDETAILS NATURAL JOIN P_PRODUCTS
            WHERE ordernumber = orderid)
  LOOP
    IF FIRST = 1 THEN
    res:= res || '"';
    ELSE
    res:= res || '", "';
    END IF;
    res:= res || p.productname;
    FIRST:= 0;
  END LOOP;
  res:= res || '"'; 
  RETURN res;
END ORDERPRINT;