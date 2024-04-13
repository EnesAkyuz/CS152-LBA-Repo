%% Extend the place predicate to include new attributes
:- dynamic place/10.

%% Redefine place to include atmosphere, internet access, power outlet access, and food availability
place(Name, Latitude, Longitude, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, PriceLevel, Photos).

%% List places (simplified to show names only for brevity)
list_places(PlacesList) :-
    findall(Name, place(Name, _, _, _, _, _, _, _, _, _), PlacesList).

%% Predicate to find the best place based on multiple criteria
find_best_place(MinRating, Latitude, Longitude, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, PriceLevel, Name, Rating) :-
    place(Name, PlaceLatitude, PlaceLongitude, Rating, PlaceAtmosphere, PlaceInternetAccess, PlacePowerOutletAccess, PlaceFoodAvailability, PlacePriceLevel, _),
    nonvar(Rating),
    Rating >= MinRating,
    Atmosphere = PlaceAtmosphere,
    InternetAccess = PlaceInternetAccess,
    PowerOutletAccess = PlacePowerOutletAccess,
    FoodAvailability = PlaceFoodAvailability,
    Latitude = PlaceLatitude,
    Longitude = PlaceLongitude,
    PriceLevel =< PlacePriceLevel.


%% Add a place dynamically with all attributes
add_place(Name, Latitude, Longitude, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, PriceLevel, Photos) :-
    (   var(Rating) -> SafeRating = 0; SafeRating = Rating ),
    assertz(place(Name, Latitude, Longitude, SafeRating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, PriceLevel, Photos)).

%% Helper to remove a place (useful for testing or updates)
remove_place(Name) :-
    retractall(place(Name, _, _, _, _, _, _, _, _, _)).
