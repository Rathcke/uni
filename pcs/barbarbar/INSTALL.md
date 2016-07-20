

Install BarbarBar
=================

0. Install Apache with PHP and a MySQL server (like `mariadb-server`),
   and a mysql CLI client (like `mariadb-client`).

1. Create an MySQL database for barbarbar, a user barman and grant
   that user all permissions on the database:

        $ mysql -h localhost -u root -p
        mysql> CREATE DATABASE barbarbar;
        mysql> GRANT ALL ON barbarbar.* TO barman IDENTIFIED BY 'secret';
        mysql> quit;

2. Install barbarbar somewhere, `$INSTDIR`, your Apache installation can find it.
   Say a subdirectory to `var/www/html` on a Debian or Ubuntu:

        sudo su
        export INSTDIR=/var/www/html/barbarbar
        mkdir $INSTDIR
        cp -r *.php static includes $INSTDIR
        find $INSTDIR -type f -exec chmod 644 {} \;
        find $INSTDIR -type d -exec chmod 755 {} \;
        chmod 755 $INSTDIR

3. Set up the database for PCS course by visiting
   <http://localhost/barbarbar/pcs_setup.php>

   You may now delete `$INSTDIR/pcs_setup.php` if you want.

4. Create a user which can run the bot.js script and move the script to its
   home folder. Then add the following line to its crontab:

        * * * * * phantomjs bot.js

5. Enjoy barbarbar on http://localhost/barbarbar



