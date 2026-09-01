import sys
import json
import time
import os

try:
    import urllib.request
    import urllib.parse
except ImportError:
    pass

def limpiar_pantalla():
    """Limpia la consola según el sistema operativo (Android/Termux/Linux o Windows)."""
    os.system('cls' if os.name == 'nt' else 'clear')

def obtener_coordenadas(ciudad):
    """Obtiene latitud y longitud de cualquier ciudad del mundo."""
    ciudad_encoded = urllib.parse.quote(ciudad)
    url = f"https://geocoding-api.open-meteo.com/v1/search?name={ciudad_encoded}&count=1&language=es&format=json"
    
    req = urllib.request.Request(url, headers={'User-Agent': 'PythonAQI/1.0'})
    try:
        with urllib.request.urlopen(req) as response:
            datos = json.loads(response.read().decode())
            if not datos.get('results'):
                return None
            res = datos['results'][0]
            return {
                'nombre': res.get('name'),
                'pais': res.get('country', ''),
                'lat': res.get('latitude'),
                'lon': res.get('longitude')
            }
    except Exception as e:
        print(f"Error de red al geocodificar: {e}")
        return None

def obtener_calidad_aire(lat, lon):
    """Consulta los índices ambientales y concentraciones en tiempo real."""
    url = (
        f"https://air-quality-api.open-meteo.com/v1/air-quality?"
        f"latitude={lat}&longitude={lon}&"
        f"current=us_aqi,pm2_5,pm10,ozone,nitrogen_dioxide,carbon_monoxide,sulphur_dioxide"
    )
    
    req = urllib.request.Request(url, headers={'User-Agent': 'PythonAQI/1.0'})
    try:
        with urllib.request.urlopen(req) as response:
            return json.loads(response.read().decode()).get('current', {})
    except Exception as e:
        print(f"Error de red al obtener datos de calidad del aire: {e}")
        return {}

def interpretar_aqi(aqi):
    """Devuelve la categoría según la escala estándar EPA (0-500)."""
    if aqi is None:
        return "N/A"
    if aqi <= 50:
        return "Bueno (Verde)"
    elif aqi <= 100:
        return "Moderado (Amarillo)"
    elif aqi <= 150:
        return "Dañino para Grupos Sensibles (Naranja)"
    elif aqi <= 200:
        return "Dañino / Insalubre (Rojo)"
    elif aqi <= 300:
        return "Muy Dañino (Morado)"
    else:
        return "Peligroso (Marrón)"

def iniciar_monitoreo(nombre_ciudad, intervalo_segundos=30):
    ubi = obtener_coordenadas(nombre_ciudad)
    if not ubi:
        print(f"[-] No se encontró la ciudad: '{nombre_ciudad}'")
        return

    print(f"Iniciando monitoreo continuo para {ubi['nombre']} cada {intervalo_segundos} segundos. Presione Ctrl+C para salir.")
    time.sleep(2)

    contador_actualizaciones = 0

    try:
        while True:
            mediciones = obtener_calidad_aire(ubi['lat'], ubi['lon'])
            
            if mediciones:
                contador_actualizaciones += 1
                
            aqi_us = mediciones.get('us_aqi')
            hora_actual = time.strftime("%H:%M:%S")

            limpiar_pantalla()
            
            print("=" * 65)
            print(f" MONITOR DE CALIDAD DEL AIRE | ACTUALIZACIÓN N°: {contador_actualizaciones}")
            print(f" ÚLTIMA LECTURA: {hora_actual}")
            print("=" * 65)
            print(f"UBICACIÓN: {ubi['nombre']}, {ubi['pais']} ({ubi['lat']}, {ubi['lon']})")
            print("=" * 65)
            print(f"Índice AQI (US):        {aqi_us} -> {interpretar_aqi(aqi_us)}")
            print("-" * 65)
            print("CONCENTRACIONES EN TIEMPO REAL:")
            print(f" - PM2.5:               {mediciones.get('pm2_5')} µg/m³")
            print(f" - PM10:                {mediciones.get('pm10')} µg/m³")
            print(f" - Ozono (O3):          {mediciones.get('ozone')} µg/m³")
            print(f" - Dióxido Nitrógeno:   {mediciones.get('nitrogen_dioxide')} µg/m³")
            print(f" - Monóxido Carbono:    {mediciones.get('carbon_monoxide')} µg/m³")
            print(f" - Dióxido Azufre:      {mediciones.get('sulphur_dioxide')} µg/m³")
            print("=" * 65)
            print(f"Próxima actualización en {intervalo_segundos} segundos. (Ctrl+C para detener)")

            time.sleep(intervalo_segundos)

    except KeyboardInterrupt:
        print(f"\n[!] Monitoreo detenido. Total de lecturas realizadas: {contador_actualizaciones}")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        ciudad_input = " ".join(sys.argv[1:])
    else:
        ciudad_input = input("Ingrese el nombre de la ciudad a monitorear: ")
    
    iniciar_monitoreo(ciudad_input, intervalo_segundos=30)