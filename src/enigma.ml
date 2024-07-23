
(** [index c] is the 0-based index of [c] in the alphabet. Requires: [c] is an
    uppercase letter in A..Z. *)
    let index (c : char) : int = Char.code (c)-65

    let modu x = if (x mod 26<0) then (x mod 26)+26 else (x mod 26);;
    
    (** [map_r_to_l wiring top_letter input_pos] is the left-hand output position at
        which current would appear when current enters at right-hand input position
        [input_pos] to a rotor whose wiring specification is given by [wiring]. The
        orientation of the rotor is given by [top_letter], which is the top letter
        appearing to the operator in the rotor's present orientation. Requires:
        [wiring] is a valid wiring specification, [top_letter] is in A..Z, and
        [input_pos] is in 0..25. *)
    let map_r_to_l (wiring : string) (top_letter : char) (input_pos : int) : int 
     =modu (index (wiring.[ input_pos + index top_letter])) - index top_letter
    (*output=left-hand position = left contact - offset *)
    (* left contact=index (wiring.[rcontact]) *)
    (*rcontact= input_pos+ offset;; offset = index top_letter  *)
    
    let alphabetChar x ="ABCDEFGHIJKLMNOPQRSTUVWXYZ".[x]
    
    (** [map_l_to_r] computes the same function as [map_r_to_l], except for current
        flowing left to right. *)
    let map_l_to_r (wiring : string) (top_letter : char) (input_pos : int) : int =
      modu (String.index wiring (alphabetChar (input_pos + (index top_letter))))-index top_letter
    
    (*after offset=a, an int -> check a in alpahbet to get corresponding char -> check position of that
       char in wiring=b, an int-> b is output after w-inverse *)
    
    (** [map_refl wiring input_pos] is the output position at which current would
        appear when current enters at input position [input_pos] to a reflector
        whose wiring specification is given by [wiring]. Requires: [wiring] is a
        valid reflector specification, and [input_pos] is in 0..25. *)
    let map_refl (wiring : string) (input_pos : int) : int =
      map_r_to_l wiring 'A' input_pos
    
    (** [map_plug plugs c] is the letter to which [c] is transformed by the
        plugboard [plugs]. Requires: [plugs] is a valid plugboard, and [c] is in
        A..Z. *)
    let rec map_plug (plugs : (char * char) list) (c : char) = 
      match plugs with
      | [] -> c
      | (h,t) :: k -> if h = c then t else if t=c then h else map_plug k c
    
    
    
    type rotor = {
      wiring : string;  (** A valid rotor wiring specification. *)
      turnover : char;(*ignore this*)
          (** The turnover of the rotor, which must be an uppercase letter. This
              field will not be used in the assignment until you implement stepping
              in the excellent scope. *)
    }
    (** [rotor] represents an Enigma rotor. *)
    
    type oriented_rotor = {
      rotor : rotor;  (** The rotor. *)
      top_letter : char;  (** The top letter showing on the rotor. *)
    }
    (** [oriented_rotor] represents a rotor that is installed on the spindle hence
        has a top letter. *)
    
    type config = {
      refl : string;  (** A valid reflector wiring specification. *)
      rotors : oriented_rotor list;
          (** The rotors as they are installed on the spindle from left to right.
              There may be any number of elements in this list: 0, 1, 2, 3, 4, 5,
              etc. The order of elements in list represents the order in which the
              rotors are installed on the spindle, **from left to right**. So, the
              head of the list is the leftmost rotor on the spindle, and the last
              element of the list is the rightmost rotor on the spindle. *)
      plugboard : (char * char) list;
          (** A valid plugboard. The order of characters in the pairs does not
              matter, and the order of pairs in the list does not matter. *)
    }
    let rec map_rotors_r_to_l (theRotors : oriented_rotor list) (i : int) : int = 
      match theRotors with
      | [] -> i
      | h :: t ->  let a=(map_r_to_l (h.rotor.wiring) (h.top_letter) i) in map_rotors_r_to_l t a
     
    let rec map_rotors_l_to_r  (theRotors: oriented_rotor list) (i:int) : int=
    match theRotors with
    | [] -> i
    | h :: t ->  let a=(map_l_to_r (h.rotor.wiring) (h.top_letter) i) in map_rotors_l_to_r t a
    
    (** [config] represents the configuration of an Enigma machine. *)
    (** [cipher_char config c] is the letter to which the Enigma machine ciphers
        input [c] when it is in configuration [config]. Requires: [config] is a
        valid configuration, and [c] is in A..Z. *)
        (*  eg: cofig.rotors gives me the actual list whose type is  oriented_rotor list*)
    let cipher_char (config : config) (c : char) : char =
      let a = map_plug (config).plugboard (c:char) in (*output goes into next function*)
      let b= (map_rotors_r_to_l (List.rev config.rotors) (index a)) in 
      let c=map_refl (config.refl) ( b) in
      let d=map_rotors_l_to_r (config.rotors) ( c) in (*index of char in rotor*)
       map_plug (config.plugboard) (alphabetChar d)
    
    
    (** [step config] is the new configuration to which the Enigma machine
        transitions when it steps beginning in configuration [config]. Requires:
        [config] is a valid configuration. *)
    let step (config : config) : config =
      raise (Failure "Unimplemented: Enigma.step")
    
    (** [cipher config s] is the string to which [s] enciphers when the Enigma
        machine begins in configuration [config]. Requires: [config] is a valid
        configuration, and [s] contains only uppercase letters. *)
    let rec cipher (config : config) (s : string) : string =
      raise (Failure "Unimplemented: Enigma.cipher")
    
    (* TODO: set the value below to the number of hours you spent working on this
       assignment, rounded to the nearest integer, then delete this TODO comment. *)
    
    let hours_worked = 12
    
    