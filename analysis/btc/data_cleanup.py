"""
Mostly a mini script to turn the data the was copied from the internet
to a standardized csv files to import easily into pandas
"""
import pandas as pd
import numpy as np
from datetime import datetime

RAW_GBTC_FILE = 'gbtc_data.txt'
RAW_BTC_FILE = 'bitcoin_data.txt'


def gbtc_line_to_date(line):
    """
    mini worker function to get date from line
    """
    return datetime.strftime(line[:13], "%m %d, %Y")


def proc_gbtc():
    """
    custom parser script for gbtc file
    """

    with open(RAW_GBTC_FILE) as fraw:
        data = {'timestamp': [],
                'open': [],
                'high': [],
                'low': [],
                'close': [],
                'volume': []
        }
        for line in fraw:
            values = line.split()
            time_str = line[:13]
            ts = gbtc_line_to_date(line)


def proc_btc():
    pass
