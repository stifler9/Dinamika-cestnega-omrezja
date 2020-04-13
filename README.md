# Dinamika-cestnega-omrezja

Avtomobili prihajajo in odhajajo iz cest. Pri tem se prilagajajo drugim vozilom na cesti in omejitvam hitrosti. Cilj je modelirati obnašanje avtomobilov na cesti. Zanimalo me bo kaj se dogaja s pretočnostjo cest v različnih situacijah.

V pomoc nam bo vir https://www.iasj.net/iasj?func=fulltext&aId=17423.

a zacetek lahko postavimo model, kjer je pospesek:
dv_1/dt = lambda*(v_2 - v_1),
kjer je lambda pozitiven faktor prilagajanja hitrosti, v_2 je hitrost vozila pred njim. Ce je vozilo pred njim vec kot 3 s varnostne razdalje naprej, se avtomobil prilagaja hitrosti omejitve ceste.

Zraven postavimo omejitev pojemka, saj avto ne more zabremzati z neomejenim pojemkom. Temu dodamo se upostevanje varnostne razdalje. Ce voznik opazi, da se ne bo mogel ustaviti na varni razdalji v primeru, da vozilo pred njim maksimalno zabremza, potem le-ta maksimalno zabremza do varne razdalje.

### Zagon:
Postavljena Shiny aplikacija (datoteka **dinamika/server.R** za zagon). Ta nam dopusca:
- da dolocamo prehodno verjetnost avtomobilov
- da spreminjamo rdeco in zelono luc na semaforju,
- da dolocamo koliko novih avtomobilov v povprecju prihaja na sekundo,
- da dolocimo omejitev hitrosti,
- da pozenemo ali ustavimo model, resetiramo
- da pospesimo cas.