
import requests
from pyswip.prolog import Prolog
import tempfile, os
from sqlalchemy import create_engine, Column, String, Float, Integer, MetaData, UniqueConstraint, JSON, Text, Boolean
from sqlalchemy.orm import declarative_base
from sqlalchemy.orm import sessionmaker
import openai
from dotenv import load_dotenv
load_dotenv()

G_API_KEY = os.getenv('G_API_KEY')
API_KEY = os.getenv('OPENAI_API_KEY')
openai.api_key = os.getenv('OPENAI_API_KEY')


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
    budget = Column(String)  # Free, <5$, 5-10$, >10$

    def __repr__(self):
        return f"<Place(name={self.name}, latitude={self.latitude}, longitude={self.longitude}, rating={self.rating}, price_level={self.price_level})>"
# Create an engine
engine = create_engine('sqlite:///places.db')

# Create all tables by issuing CREATE TABLE commands to the DB.
Base.metadata.create_all(engine)

# Create a sessionmaker bound to the engine
Session = sessionmaker(bind=engine)


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
        for place in results:
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
        print(name)
        lat = place['geometry']['location']['lat']
        lng = place['geometry']['location']['lng']
        rating = place.get('rating', 0)
        photos = place.get('photos', [{}])[0].get('photo_reference', '')
        description = place.get('description', '')
        price_level = place.get('price_level', 0)
        atmosphere = get_atmosphere(place)
        internet_access = get_internet_access(place)
        power_outlet_access = get_power_outlets(place)
        food_availability = get_food_availability(place)

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
        command = f"add_place('{name}', {lat}, {lng}, {rating}, '{atmosphere}', '{internet_access}', '{power_outlet_access}', '{food_availability}', '{photos}')"
        print(command)
        next(prolog.query(command))

    session.commit()


        

def main():
    prolog = Prolog()



    kb_path = os.path.abspath("kb.pl")
    print(kb_path)
    
    # Consult KB
    kb_path = "kb.pl"

    # Setup the database session
    session = Session()
       
    try:
        prolog.consult(kb_path)
        print("KB loaded successfully.")
    except Exception as e:
        print("Failed to load KB:", e)

    type_of_place = input("Enter the type of place you are looking for (e.g., cafe, restaurant): ")
    places = fetch_places_from_google_maps(type_of_place)
    if places:
        update_prolog_kb(prolog, places, session)
        min_rating = float(input("Enter the minimum acceptable rating (e.g., 4.5): "))

        
        query_string = f"find_best_place({min_rating}, Name, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability)"
        best_places = list(prolog.query(query_string))        
        if best_places:
            for place in best_places:
                print(f"Recommended Place: {place['Name']} with rating {place['Rating']}")
        else:
            print("No places found with rating above or equal to:", min_rating)

    else:
        print("No new places were found to update the KB.")

if __name__ == "__main__":
    main()
