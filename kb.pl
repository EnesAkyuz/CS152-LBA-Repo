%% Define the places with more details 
place(Name, Latitude, Longitude, Rating).

%% Dynamic predicate declaration
:- dynamic place/4.

list_places(PlacesList) :-
    findall(Name, place(Name, _, _, _), PlacesList).

%% Predicate to find the best place based on multiple criteria
find_best_place(MinRating, Name, Rating) :-
    place(Name, _, _, Rating),
    nonvar(Rating), 
    Rating >= MinRating.

%% Add a place dynamically
add_place(Name, Latitude, Longitude, Rating) :-
    (   var(Rating) -> SafeRating = 0; SafeRating = Rating ),
    assertz(place(Name, Latitude, Longitude, SafeRating)).

%% Helper to remove a place (useful for testing or updates)
remove_place(Name) :-
    retractall(place(Name, _, _, _)).
