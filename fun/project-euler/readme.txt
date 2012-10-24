
See http://projecteuler.net.
Username: doug_rogers
Password: dreuler

To run, first untar projecteuler.net.tar.gz.

  tar -xzf projecteuler.net.tar.gz

That's where the problems may be found/browsed. The programs should look for
any data files that they need, usually under projecteuler.net/project/.

To update projecteuler.net.tar.gz, use the following:

  wget -r -l inf --wait=0.250 -e robots=off -nc -k http://projecteuler.net
  tar -czf projecteuler.net.tar.gz projecteuler.net/

Please note the date and the last problem number in the commit message.

