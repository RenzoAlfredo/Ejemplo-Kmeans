/************************************************ 

--CREADOR: RAHH 	  
--FECHA: 12/08/2023 
		       
--OBJETIVO      

--PARAMETROS 
	        

--RETURN 
	       

--LÓGICAS/SUPUESTOS/CÁLCULOS 	         

************************************************/      

/******************* 0. Browsing de las bases recibidas ***************************/ 
select top 50* from BanMed_clientes;
select top 50* from BanMed_fracciones;
select top 50* from BanMed_montos;


select count(idcliente), count(distinct idcliente) from BanMed_clientes ;
--15786 15786
select count(idcliente), count(distinct idcliente) from BanMed_fracciones ;
--15651 15651
select count(idcliente), count(distinct idcliente) from BanMed_montos ;
--15786 15786

select* from BanMed_montos where montoglobal = 0.000;

/******************* 1. Tratamiento de la tabla  BanMed_clientes*******************/ 
drop table if exists BanMed_clientesCat;
select a.*
, case when sexo = 1 then 'M'
       when sexo = 2 then 'F'
       when sexo = 3 then 'SIN DATOS' 
       else '' end sexoCat
, case when estado_civil = 1 then 'soltero' 
when estado_civil = 2 then 'casado'
when estado_civil = 3 then 'divorciado'
when estado_civil = 4 then 'viven juntos/no estan casados'
when estado_civil = 5 then 'viudo(a)'
when estado_civil = 6 then 'otros'
when estado_civil = 7 then 'sin datos' else '' end estado_civilCat
, (year(getdate())-nacimiento)  as edadCat ---select top 10*
into BanMed_clientesCat
from BanMed_clientes a ;

select count(idcliente), count(distinct idcliente) from BanMed_clientesCat ;
--15786 15786
select top 50 * from BanMed_clientesCat;


/******************* 2. Tratamiento de la tabla  BanMed_fracciones*******************/ 
--> unpivot a la tabla  BanMed_fracciones
DROP TABLE if exists BanMed_fracciones_normalizado;
SELECT * 
INTO BanMed_fracciones_normalizado
FROM BanMed_fracciones
unpivot (monto for categoria in 
(
efectivo
,aerolineas
,agenciaviaje
,casinos
,comprasinternet
,farmacia
,gasolineras
,hoteles
,otros
,rentaautos
,restaurantes
,supermercados
,articuloselectricos
,tiendaspordepartamento
)   ) pvt;

select top 50* from BanMed_fracciones_normalizado;

sp_help BanMed_fracciones_normalizado

ALTER TABLE BDANALYTICS.DBO.BanMed_fracciones_normalizado     
ALTER COLUMN monto float; 


--quitamos los monto = 0 para no confundir el ordenamiento
delete from BanMed_fracciones_normalizado where monto = 0;

--generamos el ranking de preferencias
drop table if exists BanMed_fracciones_normalizado_rank;
select a.*
, ROW_NUMBER() over(partition by idcliente order by monto desc) nf  
into BanMed_fracciones_normalizado_rank
from BanMed_fracciones_normalizado a;

select count(idcliente), count(distinct idcliente) from BanMed_fracciones_normalizado_rank ;
-- 60910 15651

select nf, count(idcliente) qty, count(distinct idcliente) qty_distinct from BanMed_fracciones_normalizado_rank  group by nf order by 1;

select top 50* from BanMed_fracciones_normalizado_rank order by 1, nf;

/******************* 3. Tratamiento de la tabla BanMed_montos*******************/
--> unpivot a la tabla  BanMed_fracciones
DROP TABLE if exists BanMed_montos_normalizado;
SELECT * 
INTO BanMed_montos_normalizado---select top 50*
FROM BanMed_montos
unpivot (monto for categoria in 
(
efectivo
,aerolineas
,agenciaviaje
,casinos
,comprasinternet
,farmacia
,gasolineras
,hoteles
,otros
,rentaautos
,restaurantes
,supermercados
,articuloselectricos
,tiendaspordepartamento	 
)   ) pvt;

select top 50* from BanMed_montos_normalizado;

sp_help BanMed_montos_normalizado

ALTER TABLE BDANALYTICS.DBO.BanMed_montos_normalizado     
ALTER COLUMN monto NUMERIC(10,3);    
          	       	          
--quitamos los monto = 0 para no confundir el ordenamiento
delete from BanMed_montos_normalizado where monto = 0.000;

--generamos el ranking de preferencias
drop table if exists BanMed_montos_normalizado_rank;
select a.*
, ROW_NUMBER() over(partition by idcliente order by monto desc) nm  
into BanMed_montos_normalizado_rank
from BanMed_montos_normalizado a;

select count(idcliente), count(distinct idcliente) from BanMed_montos_normalizado_rank ;
--76561 15651

select nm, count(idcliente) qty, count(distinct idcliente) qty_distinct from BanMed_montos_normalizado_rank  group by nm order by 1;
select nf, count(idcliente) qty, count(distinct idcliente) qty_distinct from BanMed_fracciones_normalizado_rank  group by nf order by 1;

select  top 100* from BanMed_montos_normalizado_rank order by 1, nm;
select  top 100* from BanMed_fracciones_normalizado_rank order by 1, nf;

/******************* 4. Consolidado para algoritmo de clustering*******************/
drop table if exists BanMed_clientes_for_clustering;
select a.idcliente
, a.sexoCat
, a.estado_civilCat
, isnull(b.categoria,'no_presenta') as categoria_preferencia_uno
, isnull(c.categoria,'no_presenta') as categoria_preferencia_dos
, a.edadCat    
, isnull(b.monto,0) as partic_preferencia_uno
, isnull(m.monto,0) as monto_prefrencia_uno
, isnull(c.monto,0) as partic_preferencia_dos
, isnull(n.monto,0) as monto_prefrencia_dos 
, ISNULL(p.montoglobal,0) as montoglobal 
into BanMed_clientes_for_clustering
from BanMed_clientesCat a
left join (select* from BanMed_fracciones_normalizado_rank where nf = 1 ) b
on a.idcliente = b.idcliente
left join (select* from BanMed_fracciones_normalizado_rank where nf = 2 ) c
on a.idcliente = c.idcliente

left join (select* from BanMed_montos_normalizado_rank where nm = 1 ) m
on a.idcliente = m.idcliente
left join (select* from BanMed_montos_normalizado_rank where nm = 2 ) n
on a.idcliente = n.idcliente

left join BanMed_montos p
on a.idcliente = p.idcliente ;

select count(idcliente), count(distinct idcliente) from BanMed_clientes_for_clustering ;
--15786 15786

select top 50* from BanMed_clientes_for_clustering order by 1, 7

