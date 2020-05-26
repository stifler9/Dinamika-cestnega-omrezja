# Dinamika-cestnega-omrezja

Avtomobili prihajajo in odhajajo iz cest. Pri tem se prilagajajo drugim vozilom na cesti in omejitvam hitrosti. Cilj je modelirati obnašanje avtomobilov na cesti. Zanimalo me bo kaj se dogaja s pretočnostjo cest v različnih situacijah.

V pomoc nam bo vir https://www.iasj.net/iasj?func=fulltext&aId=17423.

Podatki o prometnih obremenitvah so pridobljeni na https://podatki.gov.si/dataset/86c39f5b-4a57-4976-a6ac-987df32c84b1/resource/5fef825a-8113-4c66-aba6-e0231a471291/download/pldp2018karta.pdf

a zacetek lahko postavimo model, kjer je pospesek:
dv_1/dt = lambda*(v_2 - v_1),
kjer je lambda pozitiven faktor prilagajanja hitrosti, v_2 je hitrost vozila pred njim. Ce je vozilo pred njim vec kot 3 s varnostne razdalje naprej, se avtomobil prilagaja hitrosti omejitve ceste.

Zraven postavimo omejitev pojemka, saj avto ne more zabremzati z neomejenim pojemkom. Temu dodamo se upostevanje varnostne razdalje. Ce voznik opazi, da se ne bo mogel ustaviti na varni razdalji v primeru, da vozilo pred njim maksimalno zabremza, potem le-ta maksimalno zabremza do varne razdalje.

### Zagon aplikacije za uvoz cest:
Za uvoz cest Shiny aplikacija (datoteka **uvozcest/server.R**) v njej skonstruiramo cestno omrezje in ga shranimo.
* (tab Ceste) Poglej si slovenske ceste na voljo.
* (tab Omrezje) Poglej kaksno je tvoje trenutno omrezje.
* (tab Pripadajoce ceste) Poglej katere ceste pripadajo dodanim vozliscem.
* (tab Ceste z enim vozliscem) Poglej katere ceste potrebujejo samo se eno vozlisce, da bodo vkljucene v omrezje.
* (tab Semaforji) Dodaj semafor/-je v zeljenih vozliscih.

Postopek:
* Izberi vozlisce, izberi koordinate v Omrezju in dodaj prvo vozlisce v Omrezje
* Cest se ni! V zavihku Ceste z enim vozliscem poglej katere ceste so lahko v naslednjem koraku v Omrezju. Po obcutku dodajaj nova vozlisca v Omrezje. Vse ceste, ki povezujejo dodana vozlisca se bodo prikazale v Omrezju.
* Ko zakljucis z dodajanjem vozlisc v zavihku Pripadajoce ceste izberi zacetne (ceste na katerih se na zacetku pojavljajo novi avti).
* Nazadnje po potrebi dodaj semafor/-je (izberi vozlisce, izberi opcijo 1, rdece luci, dodaj opcijo, izberi opcijo 2,..., shrani semafor).
* Shrani cestno omrezje in pojdi na naslednjo aplikacijo.

### Zagon dinamicne aplikacije:
Za vizualizacijo dinamike omrezja postavljena Shiny aplikacija (datoteka **dinamika/server.R** za zagon). Ta nam dopusca:
- da dolocamo prehodno verjetnost avtomobilov
- da spreminjamo rdeco in zelono luc na semaforju,
- da dolocamo koliko novih avtomobilov v povprecju prihaja na sekundo,
- da dolocimo omejitev hitrosti,
- da pozenemo ali ustavimo model, resetiramo
- da pospesimo cas.