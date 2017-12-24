errtext =: (0 : 0)"_
[PLOTTER] Installation will install all available J packages. Please supply a command line argument to confirm.
)
exit@0:@echo@errtext^:(2>:#)ARGV
echo '[PLOTTER] -----BEGIN INSTALLATION-----'
install 'all'
echo '[PLOTTER] -----END INSTALLATION-----'
echo 'Installation succeeded.'
exit 0