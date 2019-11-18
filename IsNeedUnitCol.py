"""
Dependencies.

import smtplib
import psycopg2 as pg
import re
import json
import time
import datetime as dt
from datetime import date
from calendar import monthrange
import numpy as np
from pandas import *
import pandas as pd
from sqlalchemy import create_engine
from datetime import timedelta,datetime
import unicodedata
import pycountry
"""
import utilities

"""
overloaded-like connectDB().
(while python does not allow shit like this)
options added to customize
host, port, dbname, user, password
"""

def ConnectDBForInterns(
    host = "ec2-16-50-134-232.compute-1.amazonaws.com", #16-50-50-16
    port = "5432",
    dbname = "d43mg7o903brjw", #w-v
    user = "whois_intern",
    password = "xe8udetQ" #Q-R
    ):
    pass
