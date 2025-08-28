# 🌳 Yggdrasil GUI

**Yggdrasil GUI** — graphical interface for managing the [Yggdrasil Network](https://yggdrasil-network.github.io/) service.  
The program provides convenient access to network settings and statistics via the tray menu.   

---

## 🛠 Stack
- **Lazarus** (IDE)
- **FreePascal** (Language)  

---

## 📎 Dependencies

### Any system:
- Yggdrasil Network
### 🐧 Linux:
- Qt5Pas (it should be in your package manager)
### 🪟 Windows:
- [Indy's OpenSSL](http://indy.fulgan.com/SSL/) (the 64bit DLLs must be alongside with yggdrasil-gui_windows64.exe)
  Note: you need only **ssleay32.dll** and **libeay32.dll**. They should be archived in .zip files.   

---

## 📦 Installation and launch
1. **Yggdrasil** must be [installed separately](https://yggdrasil-network.github.io/installation.html).  
2. Download the latest version of **Yggdrasil GUI** from [releases](https://github.com/EdwardBrok/yggdrasil-network-gui/releases).
3. Satisfy all dependencies for YggGUI on your system.  
4. Run the executable file (no installation is required).  

---

## 🖥 Usage
After launching, the program appears in the system tray. Available Functions:
- ⚙️ Yggdrasil configuration change  
- 🌐 View information about the node (IP, keys)  
- 📊 Statistics of active connections  
- 🔄 Restarting the service
- 📩 Importing peers from the [public peers list]('https://publicpeers.neilalexander.dev/publicnodes.json')  
- ❌ Yggdrasil stop  

---

## 📜 License
The project is distributed under the **GNU GPL 2.0** license.  
For more information, see [LICENSE](LICENSE).  

---

## 🤝 Contributing
Welcome:  
- Bug reports in **Issues**  
- Pull Requests with improvements  

---

## 📩 Contacts
- GitHub: [@EdwardBrok](https://github.com/EdwardBrok)  

---

⚠️ **Important**: The program **does not include** Yggdrasil — it must be installed separately!
