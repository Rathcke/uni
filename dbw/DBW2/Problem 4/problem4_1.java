import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.Connection;
import java.sql.Date;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

public class problem4_1 {

  public static void main(String argv[]) {

    try {

      String dbuser = "DB_119";
      String passwd = "db2013";

      Connection conn;
      int port = 1521;
      try {
        Class.forName("oracle.jdbc.driver.OracleDriver");
        conn = DriverManager.getConnection("jdbc:oracle:thin:@localhost:"
            + port + ":dbwc", dbuser, passwd);
        conn.setAutoCommit(false);

        DocumentBuilderFactory docFactory = DocumentBuilderFactory
            .newInstance();
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

        Document doc = docBuilder.newDocument();
        Element rootElement = doc.createElement("orders");
        doc.appendChild(rootElement);

        PreparedStatement s = conn
            .prepareStatement("Select ordernumber, orderdate, requireddate, shippeddate, status, customername "
                + "FROM P_ORDERS NATURAL JOIN P_CUSTOMERS");

        ResultSet rs = s.executeQuery();

        while (rs.next()) {

          String snumber = rs.getString(1);
          Date sdate = rs.getDate(2);
          Date sreqdate = rs.getDate(3);
          Date sshipdate = rs.getDate(4);
          String sstatus = rs.getString(5);
          String scustname = rs.getString(6);

          Element order = doc.createElement("order");
          rootElement.appendChild(order);
          
          order.setAttribute("number", snumber);
          order.setAttribute("date", sdate.toString());
          order.setAttribute("requireddate", sreqdate.toString());
          if (sshipdate != null) {
          order.setAttribute("shippeddate", sshipdate.toString());
          }
          order.setAttribute("status", sstatus);
          order.setAttribute("customername", scustname);

          PreparedStatement r = conn
              .prepareStatement("Select productname, quantityordered, priceeach, orderlinenumber "
                  + "FROM P_ORDERDETAILS NATURAL JOIN P_PRODUCTS "
                  + "WHERE ordernumber ='" + snumber + "'");

          ResultSet rr = r.executeQuery();

          while (rr.next()) {

            String rname = rr.getString(1);
            String rquant = rr.getString(2);
            String rprice = rr.getString(3);
            String rlinenumb = rr.getString(4);

            Element orderdetail = doc.createElement("orderdetail");
            order.appendChild(orderdetail);

            orderdetail.setAttribute("productname", rname);
            orderdetail.setAttribute("quantity", rquant);
            orderdetail.setAttribute("price", rprice);
            orderdetail.setAttribute("linenumber", rlinenumb);

          }
        }
        OutputFormat outFormat = new OutputFormat(doc);
        outFormat.setIndenting(true);

        File file = new File("problem4_1.xml");

        FileOutputStream out = new FileOutputStream(file);

        XMLSerializer serializer = new XMLSerializer(out, outFormat);

        serializer.serialize(doc);

      } catch (SQLException e) {
        System.out.println(e);
      } catch (ClassNotFoundException e) {
        System.out.println(e);
      } catch (FileNotFoundException e) {
        e.printStackTrace();
      } catch (IOException e) {
        e.printStackTrace();
      }

    } catch (ParserConfigurationException pce) {
      pce.printStackTrace();
    }
    System.out.println("Done");
  }
}