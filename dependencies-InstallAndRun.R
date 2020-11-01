# This script downloads and installs automatically the required packages to run the simulations
# Warning: This step can take several minutes

# Author: Israel Vasconcelos
# Federal University of Alagoas
# Sep, 2020

usePackage <- function(p)  {
	if (!is.element(p, installed.packages()[,1]))
		install.packages(p, dep = TRUE, repos='http://cran.us.r-project.org')
	
	suppressMessages(require(p, character.only = TRUE))
}

p <- c("gstat","geobr","hash","sp","tidyr","EnvStats")

for(i in p)
	usePackage(i)


