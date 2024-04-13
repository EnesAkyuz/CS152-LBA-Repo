import openai
import os
from dotenv import load_dotenv
import telebot
import requests
from pyswip.prolog import Prolog
from sqlalchemy import create_engine, Column, String, Float, Integer, MetaData, UniqueConstraint, JSON, Text, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker


load_dotenv()


# keys
BOT_TOKEN = os.getenv('T_TOKEN')
G_API_KEY = os.getenv('G_API_KEY')
API_KEY = os.getenv('OPENAI_API_KEY')


bot = telebot.TeleBot(BOT_TOKEN)
openai.api_key = os.getenv('OPENAI_API_KEY')

# Questions and options for the menus
questions_and_options = {
    "Select your city:": ["Seoul", "London", "Berlin", "Buenos Aires", "Hyderabad", "San Francisco", "Taipei"],
    "What type of place you are looking for?": ["Cafe", "Restaurant", "Co-working", "Bar", "Night Club"],
    "Do you prefer a quiet or lively atmosphere?": ["Quiet", "Lively"],
    "Do you require internet access?": ["Yes", "No"],
    "Do you need access to power outlets?": ["Yes", "No"],
    "Would you like to have the option to purchase food at the place? ": ["Yes", "No"],
    "What is your budget for spending?": ["Free", "Inexpensive", "Moderate", "Expensive", "Very Expensive"],
    "How many star ratings should the place have? ": ["1", "2", "3", "4", "5"]
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
    photos = Column(JSON)  # Storing JSON data
    description = Column(Text)
    price_level = Column(Integer)  # Google's price level ranging from 0 (free) to 4 (very expensive)
    atmosphere = Column(String)  # Quiet or Lively
    internet_access = Column(Boolean)  # Yes or No
    power_outlet_access = Column(Boolean)  # Yes or No
    food_availability = Column(Boolean)  # Yes or No

    def __repr__(self):
        return f"<Place(name={self.name}, latitude={self.latitude}, longitude={self.longitude}, rating={self.rating}, price_level={self.price_level})>"

# Create an engine
engine = create_engine('sqlite:///places.db')

# Create all tables by issuing CREATE TABLE commands to the DB.
Base.metadata.create_all(engine)

# Create a sessionmaker bound to the engine
Session = sessionmaker(bind=engine)

def fetch_place_photos(photo_id):
    photo_url = f"https://maps.googleapis.com/maps/api/place/photo?maxwidth=400&photoreference={photo_id}&key={G_API_KEY}"
    return photo_url

def fetch_place_details(place_id):
    details_url = f"https://maps.googleapis.com/maps/api/place/details/json?place_id={place_id}&key={G_API_KEY}"
    response = requests.get(details_url)
    if response.status_code == 200:
        return response.json().get('result', {})
    return {}

def fetch_places_from_google_maps(type_of_place, location='48.8588443,2.2943506', radius=500):
    url = f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={location}&radius={radius}&type={type_of_place}&key={G_API_KEY}"
    response = requests.get(url)
    places = []
    if response.status_code == 200:
        results = response.json().get('results', [])
        for place in results[:5]:
            details = fetch_place_details(place['place_id'])
            places.append(details)
    return places

def query_openai_for_attribute(prompt):
    try:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": prompt}
            ]
        )
        return response.choices[0].message['content'].strip()
    except Exception as e:
        print(e)
        return str(e)

def format_place_for_query(place):
    # Extract necessary details to formulate a question context
    name = place.get('name', 'This place')
    address = place.get('vicinity', 'unknown address')
    reviews = place.get('reviews', [])
    review_excerpt = reviews[0]['text'] if reviews else 'No reviews available'
    
    # Construct a concise description of the place
    place_description = f"{name} located at {address}. Notable review: {review_excerpt}"
    return place_description

def get_atmosphere(place):
    place_description = format_place_for_query(place)
    prompt = f"Is the atmosphere generally quiet or lively in the following place ['Quiet', 'Lively']?  {place_description}"
    response = query_openai_for_attribute(prompt)
    normalized_response = response.lower()
    print("Atmosphere: ", normalized_response)
    if "quiet" in normalized_response:
        return "Quiet"
    else:
        return "Lively"

def get_internet_access(place):
    place_description = format_place_for_query(place)
    prompt = f"Does this place offer internet access ['Yes', 'No']? {place_description}"
    response = query_openai_for_attribute(prompt)
    normalized_response = response.lower()
    print("Internet: ", normalized_response)
    if "yes" in normalized_response or "available" in normalized_response:
        return True
    else:
        return False

def get_power_outlets(place):
    place_description = format_place_for_query(place)
    prompt = f"Are there power outlets available for public use at the following place ['Yes', 'No']? {place_description}"
    response = query_openai_for_attribute(prompt)
    normalized_response = response.lower()
    print("Outlets: ", normalized_response)
    if "yes" in normalized_response or "available" in normalized_response:
        return True
    else:
        return False

def get_food_availability(place):
    place_description = format_place_for_query(place)
    prompt = f"Can visitors purchase food at the following place ['Yes', 'No']? {place_description}"
    response = query_openai_for_attribute(prompt)
    normalized_response = response.lower()
    print("Food: ", normalized_response)
    if "yes" in normalized_response or "available" in normalized_response:
        return True
    else:
        return False


def update_prolog_kb(prolog, places, session):
    for place in places:
        name = place.get('name', '').replace("'", '"')
        lat = place['geometry']['location']['lat']
        lng = place['geometry']['location']['lng']
        rating = place.get('rating', 0)
        photos = place.get('photos', [{}])[0].get('photo_reference', '')
        description = place.get('description', '')
        price_level = place.get('price_level', 0)  # Extract price level
        atmosphere = get_atmosphere(place)
        internet_access = get_internet_access(place)
        power_outlet_access = get_power_outlets(place)
        food_availability = get_food_availability(place)

        # Ensure existing places are updated or new entries are created
        existing_place = session.query(Place).filter_by(name=name, latitude=lat, longitude=lng).first()
        if existing_place:
            if existing_place.rating != rating or existing_place.description != description:
                existing_place.rating = rating
                existing_place.photos = photos
                existing_place.description = description
                existing_place.price_level = price_level
                existing_place.atmosphere = atmosphere
                existing_place.internet_access = internet_access
                existing_place.power_outlet_access = power_outlet_access
                existing_place.food_availability = food_availability
                session.add(existing_place)
        else:
            place_entry = Place(name=name, latitude=lat, longitude=lng, rating=rating, photos=photos,
                                description=description, price_level=price_level, atmosphere=atmosphere,
                                internet_access=internet_access, power_outlet_access=power_outlet_access, food_availability=food_availability)
            session.add(place_entry)

        # Update Prolog if necessary
        command = f"add_place('{name}', {lat}, {lng}, {rating}, '{atmosphere}', '{internet_access}', '{power_outlet_access}', '{food_availability}', {price_level}, '{photos}')"
        print(command)
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

    if current_index == 1:
        places = fetch_places_from_google_maps(call.data)
        print(places)
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
    answers = session['answers']
    min_rating = answers.get("How many star ratings should the place have? ", "1")  # Default to "1" if not specified
    atmosphere = answers.get("Do you prefer a quiet or lively atmosphere?")
    internet_access = 'True' if answers.get("Do you require internet access?") == 'Yes' else 'False'
    power_outlet_access = 'True' if answers.get("Do you need access to power outlets?") == 'Yes' else 'False'
    food_availability = 'True' if answers.get("Would you like to have the option to purchase food at the place? ") == 'Yes' else 'False'
    budget = answers.get("What is your budget for spending?", "Inexpensive")  # Default to "Inexpensive" if not specified
    
    # Map budget answers to price levels if necessary (this part needs specific implementation details)
    price_level_map = {"Free": 0, "Inexpensive": 1, "Moderate": 2, "Expensive": 3, "Very Expensive": 4}
    price_level = price_level_map.get(budget, 0)  # Default to 0 (Free) if not mapped

    query = f"find_best_place({min_rating}, '{atmosphere}', {internet_access}, {power_outlet_access}, {food_availability}, {price_level}, Name, Rating)"
    best_places = list(prolog.query(query))
    
    if best_places:
        response_text = f"Recommended places:\n"
        for place in best_places:
            try: 
                chat_query += f"{place['Name']} with rating {place['Rating']}, "
                if place['PlaceAtmosphere']:
                    chat_query += f"{place['PlaceAtmosphere']}, "
                if place['PlaceInternetAccess']:
                    chat_query += f"{place['PlaceInternetAccess']}, "
                if place['PlacePowerOutletAccess']:
                    chat_query += f"{place['PlacePowerOutletAccess']}, "
                if place['PlaceFoodAvailability']:
                    chat_query += f"{place['PlaceFoodAvailability']}"
                if place['PlacePriceLevel']:
                    chat_query += f"{place['PlacePriceLevel']}"
                chat_query += "\n"
                try: 
                    images = fetch_place_photos(place['Photos'])
                except Exception as e:
                    pass
            except Exception as e:
                pass
        try:
            response = openai.ChatCompletion.create(
                model="gpt-3.5-turbo",
                messages=[
                    {"role": "system", "content": "You are an assistant that has knowledge about various cities, at this point in the conversation I have already expressed me preferences for the type of place I am looking for and the city I am in. Now I want to create a message summarizing your findings which is this: {chat_query}, remember reply only with the response sent to the user as the text, avoid duplicates and if there are no places politely inform the user."}]
            )
            response_text = response.choices[0].message['content'].strip()
            response_text += f"images: {images}"
        except Exception as e:
            response_text = 'I apologize, but I am unable to provide a response at this time.'
    else:
        response_text = "No places found with your preferences."

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



