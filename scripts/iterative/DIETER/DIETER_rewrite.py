'''
Rewrite the gams code for each year
'''
import sys

year_list = [2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100]

filename = "DIETER_v1.0.2_1.gms"

def rewriteGAMS(i, filename_i):

	f = open(filename,"r") #read header
	lines = f.readlines()
	f.close()

	f2 = open(filename_i,"w")

	for j in range(len(lines)):
		line = lines[j]
		line = line.replace(str(year_list[1]), str(year_list[i]))
		line = line.replace(str(year_list[0]), str(year_list[i-1]))
		line = line.replace("results_DIETER_y1", "results_DIETER_y"+ str(i) )
		line = line.replace("full_DIETER_y1", "full_DIETER_y"+ str(i) )
		line = line.replace("report_DIETER_y1", "report_DIETER_y"+ str(i) )
		f2.write(line)

	f2.close()

if __name__ == "__main__":
	for i in range(2,len(year_list)):
		filename_i = "DIETER_v1.0.2_"+ str(i) + ".gms"
		rewriteGAMS(i, filename_i)




