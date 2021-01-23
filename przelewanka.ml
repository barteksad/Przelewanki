(* autor: Bartek Sadlej *)


exception Wynik of int;;

(* 
informacje o tym, czy dany stan był rozpatrzony trzymamy w hash table
stany do rozpatrzenia trzymamy w kolejce
rozpatrując każdy stan dodajemy nowe, do których możemy z aktualnego dojść
jeśli trafimy na wynik podnosimy wyjątek z wartością równą liczbie wykonanych przelewanek od stanu początkowego
*)

let oblicz_nwd a b =
    let rec euklides x y = 
        if y = 0 then x else
        euklides y (x mod y)
    in
    euklides (max a b) (min a b);;


let czy_sie_da objetosci docelowe_objetosci =
    let warunek1 = ref false in
    Array.iter2 (fun obj cel -> if cel = 0 ||  obj = cel then warunek1 := true ) objetosci docelowe_objetosci;
    if not !warunek1 then
        false
    else
        let nwd = Array.fold_left oblicz_nwd objetosci.(0) objetosci in
        if Array.exists (fun cel -> cel mod nwd != 0) docelowe_objetosci then
            false
        else
            true;;


let przelewanka a =
    try
    let n = Array.length a in

    let objetosci = Array.map fst a in
    let docelowe_objetosci =  Array.map snd a in
    if docelowe_objetosci = (Array.make n 0) then raise (Wynik 0);
    if not (czy_sie_da objetosci docelowe_objetosci) then raise (Wynik (-1));

    let sprawdzone = Hashtbl.create (1000 * n) in
    let do_sprawdzenia = Queue.create () in

    Queue.add ((Array.make n 0), 0) do_sprawdzenia;

    while not (Queue.is_empty do_sprawdzenia ) do
        let (aktualny, odleglosc) = Queue.take do_sprawdzenia in

        if not (Hashtbl.mem sprawdzone aktualny) then 
        begin
        Hashtbl.add sprawdzone aktualny true;

        
        for nr_szklanki = 0 to n-1 do
        (* dolewanie do pełna *)
            let nowy = Array.copy aktualny in
            nowy.(nr_szklanki) <- objetosci.(nr_szklanki);
            if nowy != aktualny then 
            begin
                if nowy = docelowe_objetosci then raise (Wynik (odleglosc + 1)) else

                if not (Hashtbl.mem sprawdzone nowy) then
                    Queue.push (nowy, odleglosc + 1) do_sprawdzenia;
            end; 
        (*### *)
        (* wylewanie całości *)
            let nowy = Array.copy aktualny in
            nowy.(nr_szklanki) <- 0;
            if nowy != aktualny then
            begin
                if nowy = docelowe_objetosci then raise (Wynik (odleglosc + 1)) else

                if not (Hashtbl.mem sprawdzone nowy) then
                    Queue.push (nowy, odleglosc + 1) do_sprawdzenia;
            end;
        (* ### *)
        (* przelewanie do innej *)
            if aktualny.(nr_szklanki) > 0 then 
            for i = 0 to n - 1 do
                if i != nr_szklanki then
                begin
                    let nowy = Array.copy aktualny in
                    let roznica  = min (objetosci.(i) - aktualny.(i)) aktualny.(nr_szklanki) in
                    nowy.(nr_szklanki) <- nowy.(nr_szklanki) - roznica;
                    nowy.(i) <- nowy.(i) + roznica;

                    if nowy != aktualny then
                    begin
                        if nowy = docelowe_objetosci then raise (Wynik (odleglosc + 1)) else

                    if not (Hashtbl.mem sprawdzone nowy) then
                        Queue.push (nowy, odleglosc + 1) do_sprawdzenia;
                    end;
                end;
            done;
        (* ### *)
        done;
        end
    done;
    -1;
    with (Wynik x) -> x;;
    