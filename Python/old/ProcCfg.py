#!/c/ProgramData/Anaconda2/python

import sys
import csv
import getopt

def read_cfg_as_dict(cfg_path):
    """Read the config and store the config body as a dictionary"""
    dict_list = []
    with open(cfg_path) as cfg_file:
        cfg_reader = csv.DictReader(cfg_file)
        for row in cfg_reader:
            dict_list.append(row)
    return dict_list


def give_user_options():
    """Give user some options"""
    print "You have the following options: "
    print "1. change all values under one column."
    print "2. change one value in one row and one column."


#para1: csv_dict_lst, dictionary list if the config file
#para2: column, the column which you want to change the values
#para3: new_val, the new value for the column
def chg_all_values_in_column(csv_dict_lst, column, new_val):
    for dict in csv_dict_lst:
        if column in dict.keys():
            dict[column] = new_val
    return csv_dict_lst


def proc_user_optins(dict_lst, opt):
    if dict_lst:
        print "columns: ",dict_lst[0].keys()
    if opt == 1:
        column = raw_input("Please Enter the column you want to change: ")
        new_val = raw_input("Please input the new value: ")
        res_lst = chg_all_values_in_column(dict_lst, column, new_val)
        return res_lst
    elif opt == 2:
        pass
    else:
        print "Wrong option, Start the program again!"


#The cfg is a csv file, get the header line
#return a list
def get_cfg_header(cfg_path):
    with open(cfg_path, "r") as cfg_file:
        lines = cfg_file.readlines()
    str_line = lines[0].rstrip()
    return str_line.split(',')

#Write New csv file back
def write_csv_back(out_path, res_lst, head):
    with open(out_path, 'w') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=head)
        writer.writeheader()

        for row in res_lst:
            writer.writerow(row)


def main(argv):
    inputfile = ''
    outputfile = ''
    try:
        opts, args = getopt.getopt(argv, "h:i:o:", ["ifile=", "ofile="])
    except getopt.GetoptError:
        print "ProcCfg.py -i <cfgfile> -o <newcfgfile>"
        sys.exit(2)
    for opt,arg in opts:
        if opt == '-h':
            print "ProcCfg.py -i <cfgfile> -o <newcfgfile>"
            sys.exit()
        elif opt in ("-i", "--ifile"):
            inputfile = arg
        elif opt in ("-o", "--ofile"):
            outputfile = arg

    cfg_dict = read_cfg_as_dict(inputfile)
    cfg_head = get_cfg_header(inputfile)

    give_user_options()

    option = input("Enter you choice: ")
    res_lst = proc_user_optins(cfg_dict, option)

    #Write changed csv file back to output file
    write_csv_back(outputfile, res_lst, cfg_head)

if __name__ == "__main__":
    main(sys.argv[1:])
