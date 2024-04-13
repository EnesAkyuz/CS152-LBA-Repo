import openai
import os
from dotenv import load_dotenv
import telebot
import requests
from pyswip.prolog import Prolog
from sqlalchemy import create_engine, Column, String, Float, Integer, MetaData, UniqueConstraint
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker


load_dotenv()


# keys
BOT_TOKEN = os.getenv('T_TOKEN')
API_KEY = os.getenv('G_API_KEY')


bot = telebot.TeleBot(BOT_TOKEN)
openai.api_key = os.getenv('OPENAI_API_KEY')

# Questions and options for the menus
questions_and_options = {
    "Select your city:": ["Seoul", "London", "Berlin", "Buenos Aires", "Hyderabad", "San Francisco", "Taipei"],
    "What type of place you are looking for?": ["Cafe", "Restaurant", "Co-working", "Bar", "Night Club"],
    "Do you prefer a quiet or lively atmosphere?": ["Quiet", "Lively"],
    "Do you require internet access?": ["Yes", "No"],
    "Do you need access to power outlets?": ["Yes", "No"],
    "Would you like to have the option to purchase food at the study spot? ": ["Yes", "No"],
    "What is your budget for spending at a study spot?": ["Free", "<5$", "5-10$", ">10$"],
    "How many star ratings should the study spot have? ": ["Any", "1", "2", "3", "4", "5"],
    "How long do you plan to study?": ["<2 hrs", "2-4 hrs", "4+ hrs", "Not sure"],
    "What are the modes of transport you prefer to use? ": ["Bus", "Train", "Bicycle", "Car", "No preference"],
    "How much time do you have available for commuting?": ["<5 mins", "5-15 mins", "15-30 mins", ">30"],
    "Is accessibility important to you? ": ["Yes", "No"]
}

# Dictionary to hold user session data
user_sessions = {}

# Define the base class
Base = declarative_base()

# Define the Place model
class Place(Base):
    __tablename__ = 'places'
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    latitude = Column(Float, nullable=False)
    longitude = Column(Float, nullable=False)
    rating = Column(Float)

    # Add a unique constraint to prevent duplicate entries
    __table_args__ = (UniqueConstraint('name', 'latitude', 'longitude', name='_name_location_uc'),)

    def __repr__(self):
        return f"<Place(name={self.name}, latitude={self.latitude}, longitude={self.longitude}, rating={self.rating})>"

# Create an engine
engine = create_engine('sqlite:///places.db')

# Create all tables by issuing CREATE TABLE commands to the DB.
Base.metadata.create_all(engine)

# Create a sessionmaker bound to the engine
Session = sessionmaker(bind=engine)


def fetch_places_from_google_maps(type_of_place, location='46.8588443,2.2943506', radius=1000):
    url = f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={location}&radius={radius}&type={type_of_place}&key={API_KEY}"
    response = requests.get(url)
    if response.status_code == 200:
        return response.json().get('results', [])
    return []

def update_prolog_kb(prolog, places, session):
    for place in places:
        name = place['name'].replace("'", '"')
        lat = place['geometry']['location']['lat']
        lng = place['geometry']['location']['lng']
        rating = place.get('rating', 0)

        # Check if place already exists
        existing_place = session.query(Place).filter_by(name=name, latitude=lat, longitude=lng).first()
        if existing_place:
            if existing_place.rating != rating:
                existing_place.rating = rating  # Update the rating if it has changed
                session.add(existing_place)
        else:
            # Place does not exist, so add it
            place_entry = Place(name=name, latitude=lat, longitude=lng, rating=rating)
            session.add(place_entry)

        # Update Prolog KB as before or check if it needs updating too
        command = f"assertz(place('{name}', {lat}, {lng}, {rating}))"
        next(prolog.query(command))
    
    session.commit()  # Commit the session at the end


def start_conversation(message):
    user_id = message.chat.id
    # Initialize user session
    user_sessions[user_id] = {
        'answers': {},
        'questions': list(questions_and_options.keys()),
        'current_question_index': 0
    }
    bot.send_message(user_id, "Hello Minervan! Let us start by learning about your address so that I can give you closer places:")


def ask_next_question(message, user_id):
    session = user_sessions[user_id]
    questions = session['questions']
    current_index = session['current_question_index']
    
    if current_index < len(questions):
        question = questions[current_index]
        options = questions_and_options[question]
        markup = telebot.types.InlineKeyboardMarkup(row_width=2)
        # Create a button for each option
        buttons = [telebot.types.InlineKeyboardButton(option, callback_data=option) for option in options]
        markup.add(*buttons)
        bot.send_message(user_id, question, reply_markup=markup)
    else:
        display_summary(message, user_id)


def handle_callback(call):
    user_id = call.message.chat.id
    session = user_sessions[user_id]
    question = session['questions'][session['current_question_index']]
    current_index = session['current_question_index']

    # Record the user's answer
    session['answers'][question] = call.data

    if current_index == 2:
        places = fetch_places_from_google_maps(call.data)
        update_prolog_kb(prolog, places, db_session)

        result = list(prolog.query("list_places(PlacesList)"))
        if result:
            places_list = result[0].get('PlacesList', [])
            if places_list:
                for place in places_list:
                    print(f"Place: {place}")
            else:
                print("Places list is empty.")
        else:
            print("Query returned no results.")

    
    session['current_question_index'] += 1

    

    # Acknowledge the callback
    bot.answer_callback_query(call.id, "You selected: " + call.data)

    # Ask the next question or end
    ask_next_question(call.message, user_id)


def display_summary(message, user_id):
    session = user_sessions[user_id]
    response_text = "Here's the address you provided and your responses:\n\n"
    response_text += f"Address: {session['answers'].get('Address', 'Not provided')}\n\n"

    best_places = list(prolog.query(f"find_best_place(3, Name, Rating)"))
    if best_places:
        for place in best_places:
            print(f"Recommended Place: {place['Name']} with rating {place['Rating']}")
    else:
        print("No places found with rating above or equal to:", 3)

    for question, answer in session['answers'].items():
        if question != 'Address':
            response_text += f"{question} \n {answer}\n\n"
    
    response_text += f"\n\n Recommended Place: {best_places[0]['Name']} with rating {best_places[0]['Rating']}"
    
    bot.send_message(user_id, response_text)
    # Optionally clear the session data if no longer needed
    del user_sessions[user_id]


@bot.message_handler(commands=['start'])
def send_welcome(message):
    start_conversation(message)


@bot.message_handler(
    func=lambda message: message.chat.id in user_sessions and 'Address' not in user_sessions[message.chat.id][
        'answers'])

def handle_address(message):
    user_id = message.chat.id
    user_sessions[user_id]['answers']['Address'] = message.text
    bot.send_message(user_id, "Thank you for your address. Now let's move on to some questions.")
    ask_next_question(message, user_id)


@bot.callback_query_handler(func=lambda call: True)
def callback_query(call):
    handle_callback(call)

@bot.message_handler(commands=['start', 'restart'])
def handle_start_or_restart(message):
    # Whether it's a start or a restart, treat it the same
    start_conversation(message)

def get_city_info(query):
    try:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are an assistant knowledgeable about various cities and the favorite study spots in these cities. These 7 cities are Buenos Aires, Berlin, London, San Francisco, Hyderabad, Seoul and Taipei!"},
                {"role": "user", "content": query}
            ]
        )
        return response.choices[0].message['content'].strip()
    except Exception as e:
        return str(e)

if __name__ == "__main__":
    prolog = Prolog()
    
    # Consult KB
    kb_path = "kb.pl"

    # Setup the database session
    db_session = Session()
       
    try:
        prolog.consult(kb_path)
        print("KB loaded successfully.")
    except Exception as e:
        print("Failed to load KB:", e)
    bot.polling()



