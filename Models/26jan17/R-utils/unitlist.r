#==========================================================================================#
#==========================================================================================#
#     This file contains the list of all commonly used units written in mathematical       #
# format.                                                                                  #
#------------------------------------------------------------------------------------------#
untab <<- list( cm          = "c*m"
              , cm2om2      = "c*m^2*m^{-2}"
              , cm2opl      = "c*m^2*p*l*a*n*t^{-1}"
              , cm2om2oyr   = "c*m^2*m^{-2}*y*r^{-1}"
              , deg         = "degree"
              , degC        = "degree*C"
              , degE        = "degree*E"
              , degN        = "degree*N"
              , degS        = "degree*S"
              , degW        = "degree*W"
              , empty       = "phantom(1)-phantom(1)"
              , gcokgw      = "g[C]^phantom(1)*k*g[W]^{-1}"
              , gcokgcbio   = "g[C]^phantom(1)*k*g[C[b*i*o]]^{-1}"
              , gmt         = "G*M*T"
              , gom3        = "g^phantom(1)*m^{-3}"
              , gocm3       = "g^phantom(1)*c*m^{-3}"
              , gwokg       = "g[W]^phantom(1)*k*g^{-1}"
              , hpa         = "h*P*a"
              , jom2        = "J^phantom(1)*m^{-2}"
              , K           = "K"
              , k           = "K"
              , kgcokgc     = "k*g[C]^phantom(1)*k*g[C]^{-1}"
              , kgcom2      = "k*g[C]^phantom(1)*m^{-2}"
              , kgcom2oyr   = "k*g[C]^phantom(1)*m^{-2}*y*r^{-1}"
              , kgcom3      = "k*g[C]^phantom(1)*m^{-3}"
              , kgcopl      = "k*g[C]^phantom(1)*p*l*a*n*t^{-1}"
              , kgcoployr   = "k*g[C]^phantom(1)*p*l*a*n*t^{-1}*y*r^{-1}"
              , kgom3       = "k*g^phantom(1)*m^{-3}"
              , kgwom2      = "k*g[W]^phantom(1)*m^{-2}"
              , kgwom2l     = "k*g[W]^phantom(1)*m[l*e*a*f]^{-2}"
              , kgwom2oday  = "k*g[W]^phantom(1)*m^{-2}*d*a*y^{-1}"
              , kgwom2ohr   = "k*g[W]^phantom(1)*m^{-2}*h*r^{-1}"
              , kgwom2os    = "k*g[W]^phantom(1)*m^{-2}*s^{-1}"
              , kgwom2loday = "k*g[W]^phantom(1)*m[l*e*a*f]^{-2}*d*a*y^{-1}"
              , kgwom3oday  = "k*g[W]^phantom(1)*m^{-3}*d*a*y^{-1}"
              , kgwoploday  = "k*g[W]^phantom(1)*p*l*a*n*t^{-1}*d*a*y^{-1}"
              , km          = "k*m"
              , Kmos        = "K^phantom(1)*m^phantom(1)*s^{-1}"
              , m           = "m"
              , mm          = "m*m"
              , mm2okgw     = "m*m^2*k*g[W]^{-1}"
              , mmoday      = "m*m^phantom(1)*d*a*y^{-1}"
              , mmolom2os   = "m*m*o*l^phantom(1)*m^{-2}*s^{-1}"
              , mmomo       = "m*m^phantom(1)*m*o*n*t*h^{-1}"
              , month       = "m*o*n*t*h"
              , mos         = "m^phantom(1)*s^{-1}"
              , m2om2       = "m^2*m^{-2}"
              , m2opl       = "m^2*p*l^{-1}"
              , m2lom2      = "m[l*e*a*f]^2*m^{-2}"
              , m2lopl      = "m[l*e*a*f]^2*p*l^{-1}"
              , m2wom2      = "m[w*o*o*d]^2*m^{-2}"
              , m2wopl      = "m[w*o*o*d]^2*p*l^{-1}"
              , m3wom3      = "m[W]^3*m^{-3}"
              , mmoyr       = "m*m^phantom(1)*y*r^{-1}"
              , mpa         = "M*P*a"
              , nmo.090     = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 90*m*m^phantom(1)*m*o^{-1}"
              , nmo.100     = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 100*m*m^phantom(1)*m*o^{-1}"
              , nmo.110     = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 110*m*m^phantom(1)*m*o^{-1}"
              , nmo.120     = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 120*m*m^phantom(1)*m*o^{-1}"
              , nmo.wdef    = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(W*D) > 10*m*m^phantom(1)*m*o^{-1}"
              , oneoyr      = "y*r^{-1}"
              , pa          = "P*a"
              , pc          = "'%'"
              , pcbio       = "'%'[b*i*o]"
              , pcagboyr    = "'%'[A*G*B]^phantom(1)*y*r^{-1}"
              , pcbaoyr     = "'%'[B*A]^phantom(1)*y*r^{-1}"
              , pcbiooyr    = "'%'[b*i*o]^phantom(1)*y*r^{-1}"
              , pcdbhoyr    = "'%'[D*B*H]^phantom(1)*y*r^{-1}"
              , pcoyr       = "'%'^phantom(1)*y*r^{-1}"
              , pcpopoyr    = "'%'[p*o*p]^phantom(1)*y*r^{-1}"
              , pcsat       = "'%'[S*a*t]"
              , plom2       = "p*l*a*n*t^phantom(1)*m^{-2}"
              , umolom2os   = "mu*m*o*l^phantom(1)*m^{-2}*s^{-1}"
              , umolom2los  = "mu*m*o*l^phantom(1)*m[l*e*a*f]^{-2}*s^{-1}"
              , umolcom2    = "mu*m*o*l[C]^phantom(1)*m^{-2}"
              , umolcom2os  = "mu*m*o*l[C]^phantom(1)*m^{-2}*s^{-1}"
              , umolcomol   = "mu*m*o*l[C]^phantom(1)*m*o*l^{-1}"
              , utc         = "U*T*C"
              , wom2        = "W^phantom(1)*m^{-2}"
              , wom2l       = "W^phantom(1)*m[l*e*a*f]^{-2}"
              , wopl        = "W^phantom(1)*p*l*a*n*t^{-1}"
              , yr          = "y*r"
              )#end list
#==========================================================================================#
#==========================================================================================#
