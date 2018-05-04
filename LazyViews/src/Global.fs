module Global

open Fable.Core.JsInterop
open Fable.Core
open System


type UserInfo =
  {
    UserName:string
  }



type KV = Map<string,string>
type KVL = List<string*string>
type Lc = (string * bool) list

//****************** ETAT MATERIEL  ******************

type EM =
  {
    Lc:Lc
    Commentaire:string option
    Kind:string
  }

  static member Empty =
    {
      Lc=List.Empty
      Commentaire=None
      Kind="etatMateriel"
    }

//****************** CERTIFICATE ******************

and Doc =
  {
    Id:string
    Rev:string option
    Deleted:bool option
    Pa:Pa option
    Cl:Cl option
    M:M option
    EM:EM
    CaCM:KV
    Mh:KV
    Et:KV
    Number:string
    Dte:System.DateTime option
    GoodToGo:bool
    Sp:string option
    Mop:Ope option
    Auth:Ope option
    TC:TC option
    Loc:Loc option
    Proc:Proc option
    Model:string option
    NS:string option
    Ident:string option
    Cat:string option
    User:Ud option
    Kind:string
  }
      static member Fake() =
        let now = System.DateTime.Now
        let number = sprintf "%i-%i%i-%i" now.Year now.Day now.Month 1
        {
            Id=now.ToLocalTime().ToString()
            Rev=None
            Deleted=None
            Pa=Pa.ByDefault |> Some
            Cl=Some <| Cl.Fake()
            M=Some <| M.Empty
            CaCM=Map.empty
            EM=EM.Empty
            Mh=Map.empty
            Et=Map.empty
            Number=number
            Dte=DateTime.Now |> Some
            GoodToGo=true
            Sp=Some "Procédure Fake"
            Mop = Ope.Fake |> Some
            Auth = Ope.Fake |> Some
            TC = TC.Local |> Some
            Loc= Loc.Home |> Some
            Proc=None
            Model=Some "ce modèle"
            Ident=None
            NS=Some <| Guid.NewGuid().ToString()
            Cat=None
            User=Some Ud.Fake
            Kind="certificate"
        }

    static member GetNumber (u:Ud option) (ct:Doc)=
      let now = System.DateTime.Now

      let next =
        match u with
        | Some u -> u.Counter
        | None -> failwith "OMG!"

      sprintf "%i-%i%i-%i" now.Year now.Day now.Month next

    static member getPDFName (certificate:Doc)=
      let pdfName = certificate.Number

      let pdfName =
        match certificate.Cl with
        | Some client -> sprintf "%s-%s" pdfName client.Name
        | None -> pdfName

      let pdfName =
        match certificate.Cat with
        | Some cat -> sprintf "%s-%s" pdfName cat
        | None -> pdfName

      let pdfName =
        match certificate.Model with
        | Some m -> sprintf "%s-%s" pdfName m
        | None -> pdfName

      let pdfName =
        match certificate.NS with
        | Some m -> sprintf "%s-%s" pdfName m
        | None -> pdfName

      pdfName

and Pa =
  { Id:string
    Rev:string option
    Deleted:bool option
    Name:string
    Address1:string
    Address2:string option
    ZipCode:string
    City:string
    Tel:string
    Email:string
    Kind:string
    LogoURL:string option
  }

    static member ByDefault = {
      Id="default"
      Rev=None
      Deleted=None
      Name="Yolo SAS"
      Address1="28 Yolo street"
      Address2=None
      ZipCode="444444"
      City="YoCity"
      Tel="0044321654987"
      Email="contact@yolo.happy"
      Kind="pt"
      LogoURL=None
    }

and Cl =
  { Id:string
    Rev:string option
    Deleted:bool option
    Name:string
    Address1:string
    Address2:string option
    ZipCode:string
    City:string
    Kind:string
  }
    static member Fake() =
      {
        Id="Fake"
        Rev=None
        Deleted = None
        Name = "Fake"
        Address1 = "I live in that street"
        Address2 = Some "I live there too"
        ZipCode = "78545"
        City = "Nowhere"
        Kind="cl"
      }
    static member Empty =
      {
        Id="Fake"
        Rev=None
        Deleted = None
        Name = ""
        Address1 = ""
        Address2 = None
        ZipCode = ""
        City = ""
        Kind="cl"
      }



//****************** MATERIEL  ******************


and M =
  { Id:string
    Rev:string option
    Deleted:bool option
    Designation:string
    Constructeur:string
    Modele:string option
    Kind:string
  }
    static member Empty =
      {
        Id = "Fake Id"
        Rev = None
        Deleted = None
        Designation = "top cool"
        Constructeur = "he's cool"
        Modele = Some "XYZ-123-VGR"
        Kind="m"
      }


//****************** User  ******************

and Udd =
  {
    Counter:int
    Sync:string list
  }
  static member Fake = {
    Counter=1
    Sync=[]
  }


and Ud =
  {
    Id:string
    Rev:string option
    Deleted:bool option
    Name:string
    Counter:int
    Sync:string list
    IsAdmin:bool
    UserData:Udd option
    Kind:string
  } with

    static member Fake = {
        Id= System.Guid.NewGuid().ToString()
        Rev= None
        Deleted=None
        Name="THat guy"
        Counter=0
        IsAdmin=false
        Sync=[]
        UserData=Some Udd.Fake
        Kind="u"
      }

and Ope =
  {
    Id:string
    Rev:string option
    Deleted:bool option
    Name:string
    Kind:string
  } with

    static member Fake = {
        Id= System.Guid.NewGuid().ToString()
        Rev= None
        Deleted=None
        Name="Bob"
        Kind="o"
      }

and TC =
  {
    Id:string
    Rev:string option
    Deleted:bool option
    Tp:string option
    H:string option
    Kind:string
  } with

    static member Local = {
      Id= System.Guid.NewGuid().ToString()
      Rev= None
      Deleted=None
      Tp =Some "TPP"
      H=Some "HHH"
      Kind="tc"
    }

and Loc =
  {
    Id:string
    Rev:string option
    Deleted:bool option
    Nm:string option
    Op:bool option
    Kind:string
  } with

    static member Empty = {
        Id= System.Guid.NewGuid().ToString()
        Rev= None
        Deleted=None
        Nm=None
        Op=None
        Kind="lc"
      }

    static member Home= {
        Id= System.Guid.NewGuid().ToString()
        Rev= None
        Deleted=None
        Nm=Some "At home"
        Op=None
        Kind="lc"
      }


and PTest =
  {
    Label:string
    IsOk:bool
  }

and Proc =
  {
    Id:string
    Rev:string option
    Deleted:bool option
    Name:string option
    Categories: (Ct list ) option
    Kind:string
  }
    static member Empty = {
        Id= System.Guid.NewGuid().ToString()
        Rev= None
        Deleted=None
        Name=None
        Categories=None
        Kind="p"
      }


and Ct=
  {
    Id:string
    Name:string option
    E:string list
    Cs:string list
    Ms:string list
    Ts:PTest list
    Kind:string
  } with
    static member Empty = {
        Id= System.Guid.NewGuid().ToString()
        Name=None
        E=List.empty
        Ts=List.empty
        Cs=List.Empty
        Ms=List.Empty
        Kind="ct"
      }




