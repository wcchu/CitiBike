import pandas as pd
import streamlit as st


@st.cache
def import_dat():
    dat = pd.read_csv("../data.csv")
    dat['time'] = pd.to_datetime(dat['starttime'])
    dat['timestamp'] = dat['time'].astype(int) / 10**9
    dat['tripduration'] = dat['tripduration'] / 60
    dat['wday'] = dat['time'].dt.weekday
    dat['hour'] = dat['time'].dt.hour
    dat = dat.rename(
        columns={
            'start.station.id': 'id_i',
            'start.station.name': 'sta_i',
            'start.station.latitude': 'lat_i',
            'start.station.longitude': 'lon_i',
            'end.station.id': 'id_f',
            'end.station.name': 'sta_f',
            'end.station.latitude': 'lat_f',
            'end.station.longitude': 'lon_f',
            'bikeid': 'bike',
            'usertype': 'user_type',
            'birth.year': 'birth',
            'tripduration': 'dur'
        })
    dat = dat[[
        'wday', 'hour', 'dur', 'id_i', 'sta_i', 'lat_i', 'lon_i', 'id_f',
        'sta_f', 'lat_f', 'lon_f', 'bike', 'user_type', 'birth', 'gender'
    ]]
    return dat


def get_user_type():
    return st.sidebar.selectbox('Choose a user type',
                                ['All', 'Subscriber', 'Customer'])


def run():

    # sidebar
    st.sidebar.header('Filter')
    user_type = get_user_type()

    # main panel
    st.header('Filtered data')
    st.write('User type is {}'.format(user_type))
    d = import_dat()
    d = d[(d['user_type'] == user_type) | (user_type == 'All')]
    d = d.reset_index()
    st.write(d)


if __name__ == "__main__":
    run()
