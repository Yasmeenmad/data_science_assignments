
#----------------------------------------------------------- import packages --------------------------------------------------------------
# import packages that we need
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns 

import plotly.express as px
import plotly.graph_objects as go
from jupyter_dash import JupyterDash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

#------------------------------------------------------------ load dataset ----------------------------------------------------------------
# load dataset and read it as csv file
df = pd.read_csv('https://raw.githubusercontent.com/Yasmeenmad/data_science_bootcamp/main/Week9/Scientific_Visualization_with_Python3/googleplaystore.csv')

#------------------------------------------------------------ clean dataset ---------------------------------------------------------------
# clean unwanted symbols in 'Installs', 'Size' and 'Price' columns
df['Installs']=df['Installs'].apply(lambda x: x.strip('+'))
df['Installs']=df['Installs'].apply(lambda x: x.replace(',',''))
df['Size']=df['Size'].str.replace('k','e+3')
df['Size']=df['Size'].str.replace('M','e+6')
df['Size']=df['Size'].replace('1,000+',1000)
df['Price'] =df['Price'].apply(lambda x: x.strip('$'))
df['Price'] =df['Price'].apply(lambda x: x.strip('$'))

# replace invalid values in 'Installs', 'Size' and 'Price' columns with nans
df['Price'] =df['Price'].replace('Everyone',np.nan)
df['Installs']=df['Installs'].replace('Free',np.nan)
df['Size']=df['Size'].replace('Varies with device',np.nan)

# convert the column type
df['Installs']=pd.to_numeric(df['Installs'])
df['Size']=pd.to_numeric(df['Size'])
df['Price']=pd.to_numeric(df['Price'])

# drop the invalid value from 'Type' column
df.drop(df.index[df['Type'] == '0'], inplace = True)

#------------------------------------------------------------ build app -------------------------------------------------------------------

app = JupyterDash(__name__)
app.layout = html.Div(children=[
    # All elements from the top of the page
    html.Div([
        html.H1("App Store Dashboard", style={'text-align': 'center'}),
        html.P('Select The Type Of The App:'),
        dcc.Dropdown(id="type_select",
                     options=[
                         {"label": "Free", "value": "Free"},
                         {"label": "Paid", "value": "Paid"}],
                     multi=False,
                     value="Free",
                     style={'width': "40%"}
                    ),
        html.Br(),
        html.P('Select The Category Of The App:'),
        dcc.Dropdown(id="category_select",
                     options=[
                         {"label": "BEAUTY", "value": "BEAUTY"},
                         {"label": "PARENTING", "value": "PARENTING"},
                         {"label": "COMICS", "value": "COMICS"},
                         {"label": "EVENTS", "value": "EVENTS"},
                         {"label": "ART AND DESIGN", "value": "ART_AND_DESIGN"},
                         {"label": "WEATHER", "value": "WEATHER"},
                         {"label": "LIBRARIES AND DEMO", "value": "LIBRARIES_AND_DEMO"},
                         {"label": "AUTO AND VEHICLES", "value": "AUTO_AND_VEHICLES"},
                         {"label": "HOUSE AND HOME", "value": "HOUSE_AND_HOME"},
                         {"label": "FOOD AND DRINK", "value": "FOOD_AND_DRINK"},
                         {"label": "MAPS AND NAVIGATION", "value": "MAPS_AND_NAVIGATION"},
                         {"label": "ENTERTAINMENT", "value": "ENTERTAINMENT"},
                         {"label": "EDUCATION", "value": "EDUCATION"},
                         {"label": "VIDEO PLAYERS", "value": "VIDEO_PLAYERS"},
                         {"label": "BOOKS AND REFERENCE", "value": "BOOKS_AND_REFERENCE"},
                         {"label": "DATING", "value": "DATING"},
                         {"label": "TRAVEL AND LOCAL", "value": "TRAVEL_AND_LOCAL"},
                         {"label": "SHOPPING", "value": "SHOPPING"},
                         {"label": "NEWS AND MAGAZINES", "value": "NEWS_AND_MAGAZINES"},
                         {"label": "SOCIAL", "value": "SOCIAL"},
                         {"label": "PHOTOGRAPHY", "value": "PHOTOGRAPHY"},
                         {"label": "HEALTH AND FITNESS", "value": "HEALTH_AND_FITNESS"},
                         {"label": "FINANCE", "value": "FINANCE"},
                         {"label": "LIFESTYLE", "value": "LIFESTYLE"},
                         {"label": "SPORTS", "value": "SPORTS"},
                         {"label": "COMMUNICATION", "value": "COMMUNICATION"},
                         {"label": "PERSONALIZATION ", "value": "PERSONALIZATION "},
                         {"label": "PRODUCTIVITY", "value": "PRODUCTIVITY"},
                         {"label": "BUSINESS", "value": "BUSINESS"},
                         {"label": "MEDICAL", "value": "MEDICAL"},
                         {"label": "TOOLS", "value": "TOOLS"},
                         {"label": "GAME", "value": "PHOTOGRAPHY"},
                         {"label": "FAMILY", "value": "FAMILY"}],
                     multi=False,
                     value="SOCIAL",
                     style={'width': "40%"}
                    ),
        html.Br(),
        html.P('Select The Rating Range Of The Plot:'),
        dcc.RangeSlider(
            id='my_range_slider',
            min=0,
            max=5,
            step=0.5,
            value=[0, 5]
        ),
        html.Br(),
        html.Div(id='output-container-range-slider'),
        dcc.Graph(id='first_plot', figure={})
    ]),
    # New Div for all elements in the new 'row' of the page
    html.Div([
        html.P('Write The Plot Title:'),
        dcc.Input(
            id="text1",
            type="text",
            placeholder=""
        ),
        html.Br(),
        html.Div(id='output_text'),
        
        html.P('Select The Y-Axis Column:'),
        dcc.RadioItems(id = "radiobuttons_select",
                       options=[
                           {'label': 'Size', 'value': 'Size'},
                           {'label': 'Installs', 'value': 'Installs'},
                           {'label': 'Price', 'value': 'Price'}
                       ],
                       value='Installs'
                      )]),
        html.Br(),
        dcc.Graph(id='second_plot', figure={})
])

@app.callback(
    [Output('first_plot', 'figure'),
     Output('output-container-range-slider', 'children')],
    [Input('type_select', 'value'),
     Input('category_select', 'value'),
     Input('my_range_slider', 'value')]
)

def first_graph(type_select, category_select, my_range_slider):

    
    df_copy = df.copy()
    df_copy = df_copy[df_copy["Type"] == type_select]
    df_copy = df_copy[df_copy["Category"] == category_select]

    # Plotly Express
    fig = px.histogram(df_copy,
                       x="Rating",
                       nbins=25,
                       range_x=my_range_slider,
                       color_discrete_sequence=['indianred'],
                       title = "App Ratings In App Store")
    
    range_values =  'You have selected "{}"'.format(my_range_slider)

    return fig, range_values

@app.callback(
    [Output('second_plot', 'figure'),
     Output('output_text', 'children')],
    [Input('radiobuttons_select', 'value'),
     Input("text1", "value")]
)

def second_graph(radiobuttons_select, text1):

    # Plotly Express
    fig2 = px.bar(df,
                  x = df.groupby("Content Rating")[radiobuttons_select].sum().values, # Grab labels 
                  y = df.groupby("Content Rating")[radiobuttons_select].sum().index, # Grab values
                  color_discrete_sequence=['indianred'],
                  labels={"x":"The Total", "y":radiobuttons_select}) # Re-label axis to appropriate names
    
    text_values =  'The Content Rating And "{}"'.format(text1)
    
    return fig2, text_values


if __name__ == '__main__':
    app.run_server(mode="inline", host="localhost",port=8053)