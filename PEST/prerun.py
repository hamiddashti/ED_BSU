# first read the in.dat file and grab the data
# prerun(in.dat)
import os
cwd = os.getcwd()
inopen = cwd+"/in.dat"
lines = [line.rstrip('\n') for line in open(inopen)]
modified_in = list(map(float, lines))
sla = modified_in[0] 	# This is the SLA value (line1)
vm0 = modified_in[1]	# This is V0 vlaue  (line 2)
sto_s = modified_in[2] 
q_ratio = modified_in[3]
ftr = modified_in[4]
ltr_var = modified_in[5]
gresp = modified_in[6]
cut_c = modified_in[7]
wat_c = modified_in[8]
s_mor = modified_in[9]
l_wid = modified_in[10]
var_str = modified_in[11]


confile = cwd+"/config_pest.xml"

from xml.etree import ElementTree as et
tree =et.parse(confile)
tree.find('.//SLA').text = ('\n\t\t%f\n\t' %sla)
tree.find('.//Vm0').text = ('\n\t\t%f\n\t' %vm0)
tree.find('.//stomatal_slope').text = ('\n\t\t%f\n\t' %sto_s)
tree.find('.//q').text = ('\n\t\t%f\n\t' %q_ratio)
tree.find('.//root_turnover_rate').text = ('\n\t\t%f\n\t' %ftr)
tree.find('.//leaf_turnover_rate').text = ('\n\t\t%f\n\t' %ltr_var)
tree.find('.//growth_resp_factor').text = ('\n\t\t%f\n\t' %gresp)
tree.find('.//cuticular_cond').text = ('\n\t\t%f\n\t' %cut_c)
tree.find('.//water_conductance').text = ('\n\t\t%f\n\t' %wat_c)
tree.find('.//seedling_mortality').text = ('\n\t\t%f\n\t' %s_mor)
tree.find('.//leaf_width').text = ('\n\t\t%f\n\t' %l_wid)
tree.find('.//storage_turnover_rate').text = ('\n\t\t%f\n\t' %var_str)
tree.write(confile)

