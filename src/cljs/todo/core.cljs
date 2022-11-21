(ns todo.core
  (:require [ajax.core :refer [GET]]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as rdom]))


(defonce api (r/atom {:loading true :sort :id :dir true :index 0 :result 100 :i 0}))

(defn update-state
  []
  (swap! api assoc :current (nth (partition (:result @api)  (:result @api) nil (:proxies (:data @api))) (:index @api))))

(defn get-api
  "Get the Todos from the API https://jsonplaceholder.typicode.com/todos/"
  []
  (GET "http://127.0.0.1:5500/malachie_large.json"
    {:handler (fn [res]
                (swap! api assoc :data res)
                (update-state))
     :response-format :json
     :keywords? true}))




(defn sort-api
  "Sort the data in the API by ASCENDING or DESCENDING order"
  []
  (if (:dir @api)
    (sort-by (:sort @api) > (:current @api))
    (sort-by (:sort @api) < (:current @api))))

(defn class
  "Arrow to show sort order"
  []
  (if (:dir @api) (str "fa-arrow-up") (str "fa-arrow-down")))


(defn active
  "Add active as a class to active th"
  [ref]
  (if (= (:sort @api) ref) (str "active") (str "")))



(defn tr
  "Render the <tr> element for each todos"
  [item]
  (swap! api assoc :i (inc (:i @api)))
  (let [{ip :public_ip
         id :proxy_id
         type :proxy_type
         city :city
         state :region
         ping :ping
         isp :isp
         zip :zip_code
         connection :conn_type} item]
    [:tr {:key id}
     [:th {:scope "row"} id]
     [:td ip]
     [:td type]
     [:td city]
     [:td state]
     [:td zip]
     [:td connection]
     [:td isp]
     [:td ping]
     [:td "Action"]]))


(defn search-input
  "Search inputs"
  [data]
  ;; Reset the results first 
  (let [{field :field
         placeholder :placeholder
         select? :select} data]
    (if select?
      [:select.form-control
       {:on-change #(swap! api assoc :search-keywords (-> % .-target .-value) :search-key field)}
       (case field
         :conn_type [:<> [:option {:value "wifi"} "WIFI"] [:option {:value "lte"} "LTE"]]
         :proxy_type [:<> [:option {:value "https"} "HTTPS"] [:option {:value "socks5"} "SOCKS5"]])]
      [:input.form-control {:on-change #(swap! api assoc :search-keywords (-> % .-target .-value) :search-key field :i 0)
                            :placeholder placeholder :type "text"}])))


(defn pagination []
  [:div.row
   [:div.col-12.mx-3
    [:nav {:aria-label "Page navigation example"}
     [:ul {:class "pagination"}
      [:li (if (= 0 (:index @api))  {:class "page-item  disabled"} {:class "page-item"})
       [:button {:class "page-link"
                 :on-click #(swap! api assoc :index (- (:index @api) 1))} "Previous"]]
      (map (fn [x]
             [:li {:class (if (= (:index @api) x) (str "page-item active") (str "page-item")) :key x}
              [:button {:class "page-link"
                        :key x
                        :on-click #(swap! api assoc :index x)} (+ 1 x)]]) (range (int (/ (count (:proxies (:data @api))) (:result @api)))))

      [:li (if (= (- (int (/ (count (:proxies (:data @api))) (:result @api))) 1) (:index @api))  {:class "page-item  disabled"} {:class "page-item"})
       [:button {:class "page-link"
                 :on-click #(swap! api assoc :index (+ (:index @api) 1))} "Next"]]]]]])

(defn table-head []
  [:thead
   [:tr
    [:th {:scope "col"} "#"]
    [:th {:scope "col"
          :class (active :public_ip)} [:span
                                       {:on-click #(swap! api assoc :sort :public_ip :dir (not (:dir @api)))} "Public IP  " [:i.fa {:class (class)}]]
     (search-input {:field :public_ip :placeholder "Public IP Address"})]
    [:th {:scope "col"
          :class (active :proxy_type)} [:span
                                        {:on-click #(swap! api assoc :sort :proxy_type :dir (not (:dir @api)))} "Protocol " [:i.fa {:class (class)}]]
     (search-input {:field :proxy_type :select true})]
    [:th {:scope "col"
          :class (active :city)} [:span
                                  {:on-click #(swap! api assoc :sort :city :dir (not (:dir @api)))} "City " [:i.fa {:class (class)}]]
     (search-input {:field :city :placeholder "City"})]
    [:th {:scope "col"
          :class (active :region)} [:span
                                    {:on-click #(swap! api assoc :sort :region :dir (not (:dir @api)))} "State  " [:i.fa {:class (class)}]]
     (search-input {:field :region :placeholder "State"})]
    [:th {:scope "col"
          :class (active :zip_code)
          :on-click #(swap! api assoc :sort :zip_code :dir (not (:dir @api)))} "ZIP " [:i.fa {:class (class)}]]
    [:th {:scope "col"
          :class (active :conn_type)}
     [:span {:on-click #(swap! api assoc :sort :conn_type :dir (not (:dir @api)))} "Connect " [:i.fa {:class (class)}]]
     (search-input {:field :conn_type :select true})]
    [:th {:scope "col"
          :class (active :isp)}
     [:span {:on-click #(swap! api assoc :sort :isp :dir (not (:dir @api)))} "ISP " [:i.fa {:class (class)}]]
     (search-input {:field :isp :placeholder "isp"})]
    [:th {:scope "col"
          :class (active :ping)
          :on-click #(swap! api assoc :sort :ping :dir (not (:dir @api)))} "Ping " [:i.fa {:class (class)}]]
    [:th {:scope "col"} "Action"]]])


(defn table []
  [:table {:class ".mt-5 table"}
   [:caption "All US premium proxy list"]
   [table-head]
   [:tbody
    (when (empty? (:current @api))
      [:tr [:td {:colSpan 8} [:div.loader]]])
    (map (fn [item]
           (if-not (empty? (:search-keywords @api))
            ;;  There is a search keyword
             (if (:us? @api)
               (if (and (= (str (:country_code item)) "US") (str/includes? (str/lower-case ((:search-key @api) item)) (str/lower-case (:search-keywords @api))))
                 (tr item))
               (if (str/includes? (str ((:search-key @api) item)) (str (:search-keywords @api))) (tr item)))
            ;;  (if (str/includes? (and (str/includes? (:country_code item) (:coverage @api)) (str/lower-case (str ((:search-key @api) item)))) (str (:search-keywords @api))) (tr item))
             (if (= false (:us? @api))
               (tr item)
               (if (= (str (:country_code item)) "US") (tr item)))))
         (sort-api))]])

(defn tabs []
  [:ul.nav.nav-pills.justify-content-center
   {:role "tablist"}
   [:li.nav-item
    {:role "presentation"
     :on-click #(swap! api assoc :us? true)}
    [:a
     {:aria-selected "true"
      :class (if (:us? @api) "nav-link active" "nav-link")
      :aria-controls "us"
      :role "tab"
      :href "#"
      :data-bs-toggle "tab"}
     "US"]]
   [:li
    {:role "presentation"
     :on-click #(swap! api assoc :us? false)}
    [:a
     {:aria-selected "false"
      :class (if-not (:us? @api) "nav-link active" "nav-link")
      :aria-controls "ww"
      :role "tab"
      :href "#"
      :data-bs-toggle "tab"}
     "World Wide"]]])

(defn heading []
  [:div.card-heading.text-center.pt-3 [:h2 "All US premium proxy list"]
   [:div.form-group
    [:div.row
     [:div.col-10
      [tabs]]
     [:div.col-2
      [:div.form-group
       [:label {:for "select"} "Select Range of result"]
       [:select.form-control
        {:on-change #(swap! api assoc :result (-> % .-target .-value int))}
        [:option {:value (:result @api)} (:result @api)]
        [:option {:value "10"} 10]
        [:option {:value "20"} 20]
        [:option {:value "50"} 50]
        [:option {:value "100"} 100]
        [:option {:value "200"} 200]
        [:option {:value "500"} 500]
        [:option {:value "1000"} 1000]
        [:option {:value "5000"} 5000]
        [:option {:value "10000"} 10000]]]]]]])

(defn main []
  (r/create-class
   {:component-will-mount (fn [_] (get-api))
    :component-did-update (fn [_ _] (update-state))
    :reagent-render (fn []
                      [:div.card.mt-5
                       [heading]
                       [:div.card-body
                        [table]
                        (pagination)]])}))


(defn ^:dev/after-load init! []
  (rdom/render [main] (.getElementById js/document "app")))

;; (init!)