# Dinamika-cestnega-omrezja

Avtomobili prihajajo in odhajajo iz cest. Pri tem se prilagajajo drugim vozilom na cesti in omejitvam hitrosti. Cilj je modelirati obnašanje avtomobilov na cesti. Zanimalo me bo kaj se dogaja s pretočnostjo cest v različnih situacijah.

V pomoc nam bo vir https://www.iasj.net/iasj?func=fulltext&aId=17423.

a zacetek lahko postavimo model, kjer je pospesek:
dv_1/dt = lambda*(v_2 - v_1),
kjer je lambda pozitiven faktor prilagajanja hitrosti, v_2 je hitrost vozila pred njim. Ce je vozilo pred njim vec kot 50 m naprej, se avtomobil prilagaja koncni hitrosti ceste.

Zraven postavimo omejitev pojemka, saj avto ne more zabremzati z neomejenim pojemkom. Temu dodamo se upostevanje varnostne razdalje. Ce voznik opazi, da se ne bo mogel ustaviti na varni razdalji v primeru, da vozilo pred njim maksimalno zabremza, potem le-ta maksimalno zabremza do varne razdalje.

Postavljen je semafor. Z drsnikom dolocimo od katerega do katerega casa je na semaforju prizgana rdeca luc. Ko je prizgana rdeca luc se vodilni avto obnasa, kot da je na konc poti stojec avtomobil.