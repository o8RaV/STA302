import numpy as np
import matplotlib.pyplot as plt

# Set seed for reproducibility
np.random.seed(42)

# Simulate independent reading data
days = np.arange(1, 101)
matt = np.random.normal(loc=20, scale=5, size=100)
ash = np.random.normal(loc=15, scale=5, size=100)
jacki = np.random.normal(loc=18, scale=5, size=100)
rol = np.random.normal(loc=22, scale=5, size=100)
mike = np.random.normal(loc=25, scale=5, size=100)

# Plot the data
plt.figure(figsize=(10, 5))

plt.subplot(1, 2, 1)
plt.plot(days, matt, label='Matt')
plt.plot(days, ash, label='Ash')
plt.plot(days, jacki, label='Jacki')
plt.plot(days, rol, label='Rol')
plt.plot(days, mike, label='Mike')
plt.xlabel('Days')
plt.ylabel('Pages Read')
plt.title('Pages Read per Day')
plt.legend()

plt.subplot(1, 2, 2)
plt.scatter(matt, ash, label='Matt vs. Ash')
plt.xlabel("Matt's Pages")
plt.ylabel("Ash's Pages")
plt.title("Matt vs. Ash Pages Read")
plt.legend()

# Save the plots
plt.savefig('independent_reading_simulation.png')

# Statistical tests
correlation_matt_ash = np.corrcoef(matt, ash)[0, 1]
correlation_jacki_rol = np.corrcoef(jacki, rol)[0, 1]
mean_mike = np.mean(mike)
std_rol = np.std(rol)
median_jacki = np.median(jacki)

# Print test results
print(f"Correlation between Matt and Ash: {correlation_matt_ash}")
print(f"Correlation between Jacki and Rol: {correlation_jacki_rol}")
print(f"Mean pages read by Mike: {mean_mike}")
print(f"Standard deviation of Rol's pages: {std_rol}")
print(f"Median pages read by Jacki: {median_jacki}")
