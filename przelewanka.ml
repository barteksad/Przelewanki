exception Wynik of int;;


let print_array a = 
    for i = 0 to Array.length a - 1 do
        print_int a.(i);
        print_string "  ";
    done;
    print_string "\n";;

(* let a = [|(10,0);(9,0)|];; *)
let przelewanka_ a =
    let n = Array.length a in

    let objetosci = Array.map fst a in
    let docelowe_objetosci =  Array.map snd a in
    if docelowe_objetosci = (Array.make n 0) then raise (Wynik 0);

    let sprawdzone = Hashtbl.create (1000 * n) in
    let do_sprawdzenia = Queue.create () in

    Queue.add ((Array.make n 0),0) do_sprawdzenia;

    while (not (Queue.is_empty do_sprawdzenia )) do
(* Queue.iter (fun x -> print_array (fst x)) do_sprawdzenia; *)
        let (aktualny,odleglosc) = Queue.take do_sprawdzenia in
        (* print_array aktualny; *)

        (* Queue.iter (fun x -> print_array (fst x)) do_sprawdzenia; *)
        if not (Hashtbl.mem sprawdzone aktualny) then (
        Hashtbl.add sprawdzone aktualny true;

        (* print_array aktualny; *)

        
        for nr_szklanki = 0 to n-1 do
        (* dolewanie do pełna *)
            let nowy = Array.copy aktualny in
            nowy.(nr_szklanki) <- objetosci.(nr_szklanki);
            if nowy != aktualny then 
            begin
                if nowy = docelowe_objetosci then raise (Wynik (odleglosc+1)) else

                if not (Hashtbl.mem sprawdzone nowy) then
                (
                    (* Hashtbl.replace sprawdzone nowy true; *)
                    Queue.push (nowy,odleglosc+1) do_sprawdzenia;
                    (* print_array nowy; *)

                ) 
            end; 
        (*### *)
        (* wylewanie całości *)
            let nowy = Array.copy aktualny in
            nowy.(nr_szklanki) <- 0;
            if nowy != aktualny then
            begin
                if nowy = docelowe_objetosci then raise (Wynik (odleglosc+1)) else

                if not (Hashtbl.mem sprawdzone nowy) then
                (
                    (* Hashtbl.replace sprawdzone nowy true; *)
                    Queue.push (nowy,odleglosc+1) do_sprawdzenia;
    

                ) 
            end;
        (* ### *)
        (* przelewanie do innej *)
            if aktualny.(nr_szklanki) > 0 then
            for i = 0 to n-1 do
                if i != nr_szklanki then
                begin
                    let nowy = Array.copy aktualny in
                    let roznica  = min (objetosci.(i) - aktualny.(i)) aktualny.(nr_szklanki) in
                    (* print_array aktualny;
                    print_int roznica;
                    print_array nowy; *)

                    nowy.(nr_szklanki) <- nowy.(nr_szklanki) - roznica;
                    nowy.(i) <- nowy.(i) + roznica;
                    if nowy != aktualny then
                    begin
                        if nowy = docelowe_objetosci then raise (Wynik (odleglosc+1)) else

                    if not (Hashtbl.mem sprawdzone nowy) then
                    (
                        (* Hashtbl.replace sprawdzone nowy true; *)
                        Queue.push (nowy,odleglosc+1) do_sprawdzenia;


                    ) 
                    end;
                end;
            done;
        done;

    (* Queue.iter (fun x -> print_array (fst x)) do_sprawdzenia; *)
        )
    done;
    -1;;

let przelewanka a = 
    try przelewanka_ a with (Wynik x) -> x;;