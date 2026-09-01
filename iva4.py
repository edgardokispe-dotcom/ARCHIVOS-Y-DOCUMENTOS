from datetime import datetime
import json
import os
import sys
import urllib.parse
import urllib.request


def limpiar_pantalla():
    """Limpia la consola en Android (Termux/Pydroid), Linux, Mac y Windows."""
    os.system("cls" if os.name == "nt" else "clear")


def clasificar_iva_tecnico(iva):
    """Asigna el rótulo técnico estándar de ventilación según el valor del IVA."""
    if iva < 2000:
        rotulo = "MUY MALO (Estancamiento / Acumulación Crítica)"
    elif iva < 4000:
        rotulo = "MALO (Ventilación Restringida)"
    elif iva < 6000:
        rotulo = "REGULAR (Dispersión Moderada)"
    elif iva < 10000:
        rotulo = "BUENO (Buena Capacidad de Dispersión)"
    else:
        rotulo = "EXCELENTE (Condición Óptima de Dispersión)"
    return rotulo


def calcular_iva_semanal_tecnico(latitud, longitud, nombre_ciudad=""):
    url = (
        f"https://api.open-meteo.com/v1/forecast?"
        f"latitude={latitud}&longitude={longitud}&"
        f"hourly=wind_speed_10m,boundary_layer_height&"
        f"wind_speed_unit=ms&forecast_days=7"
    )

    req = urllib.request.Request(
        url, headers={"User-Agent": "Python-IVA-Tecnico/1.0"}
    )

    try:
        with urllib.request.urlopen(req) as response:
            data = json.loads(response.read().decode("utf-8"))

            tiempos = data["hourly"]["time"]
            h_mezcla_list = data["hourly"]["boundary_layer_height"]
            v_viento_list = data["hourly"]["wind_speed_10m"]

            ahora = datetime.now()

            print("=" * 95)
            print(
                f"EVALUACIÓN TÉCNICA DEL ÍNDICE DE VENTILACIÓN (IVA) - PRONÓSTICO 7 DÍAS"
            )
            if nombre_ciudad:
                print(f"Ubicación: {nombre_ciudad}")
            print(f"Coordenadas: Lat {latitud}, Lon {longitud}")
            print("=" * 95)
            print(
                f"{'Fecha y Hora':<16} | {'Hm (m)':<8} | {'Vv (m/s)':<8} | {'IVA (m²/s)':<10} | {'Rótulo Técnico'}"
            )
            print("-" * 95)

            for i, t_str in enumerate(tiempos):
                fecha_hora = datetime.strptime(t_str, "%Y-%m-%dT%H:%M")

                # Inicia el reporte desde la hora actual de ejecución
                if fecha_hora >= ahora.replace(
                    minute=0, second=0, microsecond=0
                ):
                    hm = h_mezcla_list[i] if h_mezcla_list[i] is not None else 0
                    vv = v_viento_list[i] if v_viento_list[i] is not None else 0

                    iva = hm * vv
                    rotulo = clasificar_iva_tecnico(iva)

                    print(
                        f"{fecha_hora.strftime('%Y-%m-%d %H:%M'):<16} | {hm:<8.1f} | {vv:<8.2f} | {iva:<10.1f} | {rotulo}"
                    )

            print("=" * 95)

    except Exception as e:
        print(f"Error al procesar el pronóstico meteorológico: {e}")


def buscar_coordenadas_ciudad(nombre_ciudad):
    """Obtiene latitud, longitud y país de cualquier ciudad del mundo."""
    ciudad_encoded = urllib.parse.quote(nombre_ciudad)
    url = f"https://geocoding-api.open-meteo.com/v1/search?name={ciudad_encoded}&count=5&language=es&format=json"

    req = urllib.request.Request(
        url, headers={"User-Agent": "Python-Geocoding/1.0"}
    )

    try:
        with urllib.request.urlopen(req) as response:
            data = json.loads(response.read().decode("utf-8"))

            if "results" not in data or len(data["results"]) == 0:
                print(
                    f"\nNo se encontraron resultados para: '{nombre_ciudad}'"
                )
                return None

            resultados = data["results"]

            # Si hay múltiples coincidencias, se despliega menú de selección
            if len(resultados) > 1:
                print("\nCoincidencias encontradas:")
                for idx, res in enumerate(resultados):
                    pais = res.get("country", "N/A")
                    region = res.get("admin1", "")
                    print(
                        f"[{idx + 1}] {res['name']} ({region}, {pais}) -> Lat: {res['latitude']}, Lon: {res['longitude']}"
                    )

                opcion = input(
                    "\nSeleccione el número de la ciudad deseada (default 1): "
                ).strip()
                opcion_idx = int(opcion) - 1 if opcion.isdigit() else 0

                if opcion_idx < 0 or opcion_idx >= len(resultados):
                    opcion_idx = 0

                eleccion = resultados[opcion_idx]
            else:
                eleccion = resultados[0]

            pais = eleccion.get("country", "")
            region = eleccion.get("admin1", "")
            detalle = f"{eleccion['name']}"
            if region:
                detalle += f", {region}"
            if pais:
                detalle += f" ({pais})"

            return eleccion["latitude"], eleccion["longitude"], detalle

    except Exception as e:
        print(f"Error en la Búsqueda de la Ciudad: {e}")
        return None


if __name__ == "__main__":
    while True:
        limpiar_pantalla()
        print("=" * 55)
        print(" BUSCADOR GLOBAL DE CIUDADES - IVA")
        print("=" * 55)
        entrada_ciudad = input(
            "Ingrese el nombre de la ciudad (o escriba 'salir'): "
        ).strip()

        if entrada_ciudad.lower() in ["salir", "exit", "0"]:
            print("\nSaliendo del programa...")
            break

        if entrada_ciudad:
            res = buscar_coordenadas_ciudad(entrada_ciudad)
            if res:
                lat, lon, nombre_completo = res
                limpiar_pantalla()
                calcular_iva_semanal_tecnico(lat, lon, nombre_completo)
        else:
            limpiar_pantalla()
            print("Entrada vacía. Ejecutando con ciudad por defecto (Lima)...")
            calcular_iva_semanal_tecnico(-12.0464, -77.0428, "Lima (Perú)")

        print("\n")
        continuar = input(
            "¿Desea buscar otra ciudad? [S/n]: "
        ).strip().lower()
        if continuar in ["n", "no"]:
            print("\nSaliendo del programa...")
            break