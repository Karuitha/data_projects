library(reticulate)
install_python(version = "3.10.8")
virtualenv_create("my_python", version = "3.10.8")

??virtualenv_install
virtualenv_install(envname = "my_python", packages = "pandas")
virtualenv_install(envname = "my_python", 
                   packages = c("matplotlib", "numpy", "geopandas", "openpyxl"))

use_virtualenv(virtualenv = "my_python", required = TRUE)



