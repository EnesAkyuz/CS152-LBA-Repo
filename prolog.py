
import requests
from pyswip.prolog import Prolog
import tempfile, os
from sqlalchemy import create_engine, Column, String, Float, Integer, MetaData, UniqueConstraint
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
import os

API_KEY = os.getenv('G_API_KEY')

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

        

def main():
    prolog = Prolog()
    
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

        
        best_places = list(prolog.query(f"find_best_place({min_rating}, Name, Rating)"))
        if best_places:
            for place in best_places:
                print(f"Recommended Place: {place['Name']} with rating {place['Rating']}")
        else:
            print("No places found with rating above or equal to:", min_rating)

    else:
        print("No new places were found to update the KB.")

if __name__ == "__main__":
    main()
