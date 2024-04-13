%% Extend the place predicate to include new attributes
:- dynamic place/9.

%% Redefine place to include atmosphere, internet access, power outlet access, and food availability
place(Name, Latitude, Longitude, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, Photos).

%% List places (simplified to show names only for brevity)
list_places(PlacesList) :-
    findall(Name, place(Name, _, _, _, _, _, _, _, _), PlacesList).

% Predicate to find the best place based on multiple criteria
find_best_place(MinRating, Name, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability) :-
    place(Name, _, _, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, _),
    nonvar(Rating),
    Rating >= MinRating.

%% Add a place dynamically with new attributes
add_place(Name, Latitude, Longitude, Rating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, Photos) :-
    (   var(Rating) -> SafeRating = 0; SafeRating = Rating ),
    assertz(place(Name, Latitude, Longitude, SafeRating, Atmosphere, InternetAccess, PowerOutletAccess, FoodAvailability, Photos)).

%% Helper to remove a place (useful for testing or updates)
remove_place(Name) :-
    retractall(place(Name, _, _, _, _, _, _, _, _)).
