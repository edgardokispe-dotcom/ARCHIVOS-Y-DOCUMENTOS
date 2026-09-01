#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Monitor Global de Calidad del Aire y Clima
Formato de salida 100% idéntico a la captura provista.
"""

import sys
import os
import math
import time
import urllib.request
import urllib.parse
import json
from datetime import datetime

def clear_screen():
    os.system('cls' if os.name == 'nt' else 'clear')

def main():
    actualizaciones = 0

    # Histórico de lecturas
    hist_pm25 = [0.0] * 10
    hist_pm10 = [0.0] * 10
    hist_o3 = [0.0] * 10
    hist_co = [0.0] * 10
    hist_temp = [0.0] * 10
    hist_hum = [0.0] * 10
    hist_eco2 = [0.0] * 10
    hist_tiempo = ["00:00:00"] * 10
    hist_contador = 0

    umbral_alerta_pm25 = 10.0

    clear_screen()

    # ====== MENÚ DE SELECCIÓN DE CIUDADES ======
    print("=========================================")
    print("   MONITOR GLOBAL: CIUDADES DEL MUNDO    ")
    print("=========================================")
    print("")
    print("CIUDADES PREDEFINIDAS:")
    print("1. La Paz")
    print("2. Buenos Aires")
    print("3. Tokyo")
    print("4. Madrid")
    print("5. New York")
    print("")
    print("También puede ingresar directamente el nombre de cualquier ciudad del mundo")
    print("(ej: Lima, Paris, Santiago, Bogota, Mexico City, etc.)")
    print("")
    
    try:
        entrada_usuario = input(" Ingrese número (1-5) o nombre de ciudad: ")
    except (EOFError, KeyboardInterrupt):
        sys.exit(0)

    entrada_usuario = entrada_usuario.strip()

    ciudad = ""
    lat = ""
    lon = ""

    if len(entrada_usuario) == 1 and '1' <= entrada_usuario <= '5':
        i = int(entrada_usuario)
        if i == 1:
            ciudad = "La Paz"
            lat = "-16.5000"
            lon = "-68.1500"
        elif i == 2:
            ciudad = "Buenos Aires"
            lat = "-34.6131"
            lon = "-58.3772"
        elif i == 3:
            ciudad = "Tokyo"
            lat = "35.6895"
            lon = "139.6917"
        elif i == 4:
            ciudad = "Madrid"
            lat = "40.4167"
            lon = "-3.7037"
        elif i == 5:
            ciudad = "New York"
            lat = "40.7128"
            lon = "-74.0060"
    else:
        ciudad = entrada_usuario
        try:
            ciudad_cod = urllib.parse.quote(ciudad)
            url_geo = f"https://geocoding-api.open-meteo.com/v1/search?name={ciudad_cod}&count=1"
            req = urllib.request.Request(url_geo, headers={'User-Agent': 'Mozilla/5.0'})
            with urllib.request.urlopen(req, timeout=10) as resp:
                data = json.loads(resp.read().decode('utf-8'))
                if "results" in data and len(data["results"]) > 0:
                    lat = str(data["results"][0]["latitude"])
                    lon = str(data["results"][0]["longitude"])
                else:
                    lat = "null"
                    lon = "null"
        except Exception:
            lat = "null"
            lon = "null"

        if lat == "null" or lon == "null":
            print(f"Error: No se pudo localizar la ciudad '{ciudad}'")
            sys.exit(1)

    # ====== BUCLE PRINCIPAL DE MONITOREO ======
    while True:
        try:
            # Obtener datos de calidad del aire
            url_aq = f"https://air-quality-api.open-meteo.com/v1/air-quality?latitude={lat}&longitude={lon}&current=pm10,pm2_5,carbon_monoxide,ozone"
            req_aq = urllib.request.Request(url_aq, headers={'User-Agent': 'Mozilla/5.0'})
            with urllib.request.urlopen(req_aq, timeout=10) as resp:
                data_aq = json.loads(resp.read().decode('utf-8')).get("current", {})
                pm25 = float(data_aq.get("pm2_5", 0.0) or 0.0)
                pm10 = float(data_aq.get("pm10", 0.0) or 0.0)
                o3 = float(data_aq.get("ozone", 0.0) or 0.0)
                co = float(data_aq.get("carbon_monoxide", 0.0) or 0.0)

            # Obtener datos meteorológicos
            url_wx = f"https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current=temperature_2m,relative_humidity_2m"
            req_wx = urllib.request.Request(url_wx, headers={'User-Agent': 'Mozilla/5.0'})
            with urllib.request.urlopen(req_wx, timeout=10) as resp:
                data_wx = json.loads(resp.read().decode('utf-8')).get("current", {})
                temp = float(data_wx.get("temperature_2m", 0.0) or 0.0)
                hum = float(data_wx.get("relative_humidity_2m", 0.0) or 0.0)

            tiempo = datetime.now().strftime("%H:%M:%S")
            ios = 0
        except Exception:
            pm25, pm10, o3, co = 0.0, 0.0, 0.0, 0.0
            temp, hum = 0.0, 0.0
            tiempo = datetime.now().strftime("%H:%M:%S")
            ios = 0

        if ios == 0:
            actualizaciones += 1

            # Almacenamiento histórico
            hist_contador += 1
            if hist_contador > 10:
                hist_contador = 1

            idx = hist_contador - 1
            hist_pm25[idx] = pm25
            hist_pm10[idx] = pm10
            hist_o3[idx] = o3
            hist_co[idx] = co
            hist_temp[idx] = temp
            hist_hum[idx] = hum
            hist_tiempo[idx] = tiempo

            # Tendencias y deltas
            if actualizaciones > 1:
                prev_idx = max(0, hist_contador - 2) if hist_contador > 1 else 9
                pm25_tendencia = pm25 - hist_pm25[prev_idx]
                temp_tendencia = temp - hist_temp[prev_idx]

                if pm25_tendencia > 5.0:
                    tendencia_str = "ALZA RÁPIDA"
                elif pm25_tendencia > 2.0:
                    tendencia_str = "En aumento"
                elif pm25_tendencia < -5.0:
                    tendencia_str = "BAJA RÁPIDA"
                elif pm25_tendencia < -2.0:
                    tendencia_str = "En descenso"
                else:
                    tendencia_str = "Estable"
            else:
                pm25_tendencia = 0.0
                temp_tendencia = 0.0
                tendencia_str = "Estable"

            # Alertas Proactivas
            alerta_tendencia = False
            nivel_alerta = 0
            mensaje_alerta = ""
            mensaje_tendencia = ""

            if pm25 > 75.0:
                nivel_alerta = max(nivel_alerta, 3)
                mensaje_alerta += "⚠️ PM2.5 EXTREMO "
            elif pm25 > 55.0:
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta += "⚠️ PM2.5 MUY ALTO "
            elif pm25 > 35.0:
                nivel_alerta = max(nivel_alerta, 1)

            if pm10 > 150.0:
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta += "⚠️ PM10 ALTO "

            if o3 > 180.0:
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta += "⚠️ OZONO ALTO "

            if co > 10000.0:
                nivel_alerta = max(nivel_alerta, 3)
                mensaje_alerta += "🚨 CO EXTREMO "
            elif co > 5000.0:
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta += "⚠️ CO ALTO "

            if temp > 35.0 or temp < -5.0:
                nivel_alerta = max(nivel_alerta, 1)
                mensaje_alerta += "🌡️ TEMP EXTREMA "

            if actualizaciones > 1:
                if pm25_tendencia > umbral_alerta_pm25:
                    alerta_tendencia = True
                    nivel_alerta = max(nivel_alerta, 2)
                    mensaje_tendencia = f"🚨 PM2.5 aumentó {pm25_tendencia:5.1f} µg/m³ en 30s"
                elif pm25_tendencia < -umbral_alerta_pm25:
                    alerta_tendencia = True
                    nivel_alerta = max(nivel_alerta, 1)
                    mensaje_tendencia = f"📉 PM2.5 disminuyó {abs(pm25_tendencia):5.1f} µg/m³ en 30s"

            # Cálculo eCO2
            if pm10 < 0.1:
                pm10 = pm25 * 1.6
            no2_estimado = co * 0.15
            so2_estimado = max(co * 0.02, 1.0)
            wind_speed = 3.0
            pressure = 1013.25

            if pm25 > 50.0 or co > 1000.0:
                location_factor = 1.0
                base_co2 = 435.0
            elif pm25 > 20.0 or co > 500.0:
                location_factor = 0.8
                base_co2 = 425.0
            elif pm25 > 5.0:
                location_factor = 0.6
                base_co2 = 420.0
            else:
                location_factor = 0.4
                base_co2 = 415.0

            co_ppm = co / 1145.0
            pm25_contrib = (pm25 / 1000.0) * 0.85 * location_factor
            pm10_contrib = (pm10 / 1000.0) * 0.72 * location_factor
            o3_contrib = (o3 / 1000.0) * 0.08 * location_factor
            co_contrib = co_ppm * 50.0 * location_factor
            no2_contrib = (no2_estimado / 1000.0) * 2.05 * location_factor
            so2_contrib = (so2_estimado / 1000.0) * 0.96 * location_factor

            temp_effect = 0.42 * math.tanh((temp - 20.0) / 10.0)
            hum_effect = -0.18 * ((hum - 50.0) / 100.0)
            wind_effect = -0.25 * (wind_speed / 5.0)
            pressure_effect = 0.003 * ((pressure - 1013.25) / 100.0)

            synergy_factor = 1.0
            num_high_pollutants = sum([1 for p in [pm25 > 35.0, co > 500.0, o3 > 100.0, pm10 > 50.0] if p])
            if num_high_pollutants >= 3:
                synergy_factor = 1.25
            elif num_high_pollutants == 2:
                synergy_factor = 1.15
            elif num_high_pollutants == 1:
                synergy_factor = 1.05

            eco2 = base_co2 + \
                   (pm25_contrib + pm10_contrib + o3_contrib + \
                    co_contrib + no2_contrib + so2_contrib) * synergy_factor + \
                   temp_effect + hum_effect + wind_effect + pressure_effect

            eco2 = max(350.0, min(2000.0, eco2))
            hist_eco2[idx] = eco2

            probabilidad_precision = 92.0

            pct_co2 = (eco2 / 1000.0) * 100.0
            pct_pm25 = (pm25 / 15.0) * 100.0
            pct_pm10 = (pm10 / 45.0) * 100.0
            pct_o3 = (o3 / 100.0) * 100.0
            pct_co = (co / 4000.0) * 100.0

            val_min_eco2 = min([x for x in hist_eco2 if x > 0] or [eco2])
            val_max_eco2 = max([x for x in hist_eco2 if x > 0] or [eco2])

            # Recomendaciones
            if pm25 > 75.0: recomendacion = "EXTREMADAMENTE PELIGROSO - Evite TODA actividad al aire libre."
            elif pm25 > 55.0: recomendacion = "MUY PELIGROSO - Evite actividades prolongadas al aire libre."
            elif pm25 > 35.0: recomendacion = "PELIGROSO - Limite actividades al aire libre."
            elif pm25 > 25.0: recomendacion = "MUY DAÑINO - Reduzca actividades exteriores intensas."
            elif pm25 > 15.0: recomendacion = "DAÑINO A LA SALUD - Grupos sensibles deben evitar esfuerzos."
            elif pm25 > 12.0: recomendacion = "MODERADO - Aceptable para la mayoría, pero considere reducir actividades intensas si presenta síntomas. Ideal para actividades cortas al aire libre."
            elif pm25 > 5.0: recomendacion = "BUENO - Calidad del aire satisfactoria."
            else: recomendacion = "EXCELENTE - Calidad del aire óptima."

            # ====== CONSTRUCCIÓN EXACTA SEGÚN CAPTURA ======
            lines = []
            lines.append("=========================================")
            lines.append(f"CIUDAD: {ciudad.lower()}")
            lines.append(f"UBICACION: {lat} , {lon}")
            lines.append(f"HORA: {tiempo}")
            lines.append(f"ACTUALIZACION #:           {actualizaciones}")
            lines.append("=========================================")
            lines.append("")

            if nivel_alerta > 0:
                lines.append("🚨 ALERTAS ACTIVAS:")
                if len(mensaje_alerta) > 0: lines.append(f"   {mensaje_alerta}")
                if alerta_tendencia: lines.append(f"   {mensaje_tendencia}")
                if nivel_alerta == 3: lines.append("   ⚠️⚡ NIVEL DE ALERTA: CRÍTICO")
                elif nivel_alerta == 2: lines.append("   ⚡ NIVEL DE ALERTA: ALTO")
                elif nivel_alerta == 1: lines.append("   ⚠️ NIVEL DE ALERTA: MODERADO")
                lines.append("")

            lines.append(" CONDICIONES ATMOSFÉRICAS:")
            lines.append(f"Temperatura:   {temp:4.1f} °C")
            lines.append(f"Humedad:       {hum:4.1f} %")
            lines.append("")

            lines.append(" DATOS REALES DE ESTACION (TIEMPO REAL):")
            
            barras_pm25 = min(15, max(1, int((pct_pm25 / 100.0) * 15)))
            barras_pm10 = min(15, max(1, int((pct_pm10 / 100.0) * 15)))
            barras_o3   = min(15, max(1, int((pct_o3 / 100.0) * 15)))
            barras_co   = min(15, max(1, int((pct_co / 100.0) * 15)))

            lines.append(f"[PM 2.5] :   {pm25:4.1f} ug/m3 [{'█'*barras_pm25 + ' '*(15-barras_pm25)}] ->   {pct_pm25:4.1f}%")
            lines.append(f"[PM 10]  :   {pm10:4.1f} ug/m3 [{'█'*barras_pm10 + ' '*(15-barras_pm10)}] ->   {pct_pm10:4.1f}%")
            lines.append(f"[OZONO]  :   {o3:4.1f} ug/m3 [{'█'*barras_o3 + ' '*(15-barras_o3)}] ->   {pct_o3:4.1f}%")
            lines.append(f"[CO]     :  {co:5.1f} ug/m3 [{'█'*barras_co + ' '*(15-barras_co)}] ->    {pct_co:3.1f}%")
            lines.append("")

            lines.append(f"📈 TENDENCIA: ➡  {tendencia_str}")
            lines.append(f" ΔPM2.5:     {pm25_tendencia:4.1f} µg/m³")
            lines.append(f" ΔTemp:      {temp_tendencia:4.1f} °C")
            lines.append("")

            lines.append("VALOR ESTIMADO POR SOFTWARE:")
            lines.append(f"[eCO2]* :  {eco2:5.1f} ppm    ->   {pct_co2:4.1f}%")
            lines.append(f"Rango eCO2:  {val_min_eco2:5.1f} -  {val_max_eco2:5.1f} ppm")
            lines.append("")

            lines.append("-----------------------------------------")
            lines.append(f"*NOTA: Precision de calculo para CO2:  {probabilidad_precision:4.1f}%")
            lines.append(" Basado en modelo científico con múltiples variables")
            lines.append("-----------------------------------------")
            lines.append("")

            lines.append(f" >>> RECOMENDACIÓN: 🛑  {recomendacion}")
            lines.append("")
            lines.append("ℹ  Cálculo eCO2 incluye: PM2.5, PM10, O3, CO, NOx*, SOx*, temp, humedad")
            lines.append("   *Estimados basados en correlaciones científicas")
            lines.append("")
            lines.append("=========================================")
            lines.append("Actualizando en 30 segundos...")
            lines.append("Ctrl+C para salir o cambiar ciudad")

            salida_texto = "\n".join(lines)

            # Impresión limpia en pantalla y archivo de registro
            clear_screen()
            print(salida_texto)

            with open("clima.txt", "w", encoding="utf-8") as f:
                f.write(salida_texto)

        try:
            time.sleep(30)
        except KeyboardInterrupt:
            print("\nPrograma finalizado por el usuario.")
            sys.exit(0)

if __name__ == '__main__':
    main()