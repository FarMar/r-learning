#    Highland Statistics Ltd.
#    www.highstat.com
   
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



#You need to be online for this.

#In R-studio: Execute the following code
#Or in the text editor of R: Copy and paste the following code into the R console

Install <- TRUE 
toInstall <- c("ggplot2", "ggmap", "plyr", "lme4", "rgl")

if(Install){
	install.packages(toInstall, 
	                 dependencies = TRUE, 
	                 repos = "http://cran.us.r-project.org")
	        }
library(ggplot2)
library(ggmap)
library(plyr)
