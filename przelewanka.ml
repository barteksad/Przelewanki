(* 
let stworz_poczatkowa n = 
    let rec loop acc i = 
        if i = n then 
            acc
        else
            loop (0::acc) (i+1)
    in
    loop [0] 1;; *)

exception Wynik of int;;


let przelewanka a =
    try

    let objetosci = Array.map fst a in
    let docelowe_objetosci =  Array.map snd a in

    let n = Array.length a in
    let sprawdzone = Hashtbl.create (10000 * n) in
    let do_sprawdzenia = Queue.create () in

    Queue.add ((Array.make n 0),0) do_sprawdzenia;
    Hashtbl.replace sprawdzone (Array.make n 0) true;

    while not (Queue.is_empty do_sprawdzenia) do
        let (aktualny,odleglosc) = Queue.take do_sprawdzenia in

        for nr_szklanki = 0 to n-1 do
        (* dolewanie do pełna *)
            let nowy = Array.copy aktualny in
            nowy.(nr_szklanki) <- objetosci.(nr_szklanki);
            if nowy != aktualny then
            begin
                if nowy = docelowe_objetosci then raise (Wynik (odleglosc+1)) else

                if Hashtbl.find_opt sprawdzone nowy = None then
                    Hashtbl.replace sprawdzone nowy true;
                    Queue.add (nowy,odleglosc+1) do_sprawdzenia;
            end;
        (*### *)
        (* wylewanie całości *)
            let nowy = Array.copy aktualny in
            nowy.(nr_szklanki) <- 0;
            if nowy != aktualny then
            begin
                if nowy = docelowe_objetosci then raise (Wynik (odleglosc+1)) else

                if Hashtbl.find_opt sprawdzone nowy = None then
                    Hashtbl.replace sprawdzone nowy true;
                    Queue.add (nowy,odleglosc+1) do_sprawdzenia;
            end;
        (* ### *)
        (* przelewanie do innej *)
            for i = 0 to n-1 do
                let roznica  = min (objetosci.(i) - aktualny.(i)) aktualny.(nr_szklanki) in
                let nowy = Array.copy aktualny in
                nowy.(i) <- objetosci.(i);
                nowy.(nr_szklanki) <- nowy.(nr_szklanki) - roznica;
                if nowy != aktualny then
                begin
                    if nowy = docelowe_objetosci then raise (Wynik (odleglosc+1)) else

                    if Hashtbl.find_opt sprawdzone nowy = None then
                        Hashtbl.replace sprawdzone nowy true;
                        Queue.add (nowy,odleglosc+1) do_sprawdzenia;
                end;
            done;
        done;
    done;
    -1
    with (Wynik x) -> x;;