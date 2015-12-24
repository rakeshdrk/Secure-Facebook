import java.security.{KeyFactory, PublicKey}
import java.security.spec.X509EncodedKeySpec
import java.util
import java.util.{HashMap, UUID}
import java.util.concurrent.ConcurrentHashMap
import org.apache.commons.codec.binary.Base64


import akka.actor.{Actor, ActorLogging, ActorSystem, Props, actorRef2Scala}
import akka.pattern.ask
import akka.routing.SmallestMailboxPool
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import spray.http.{HttpCookie, MediaTypes}
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.Directive.pimpApply
import spray.routing.{Route, SimpleRoutingApp}

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParHashMap
import scala.concurrent.Await

object FacebookServer extends App with SimpleRoutingApp {

  val actorConf =
    """
  akka {
    #log-config-on-start = on
    stdout-loglevel = "INFO"
    loglevel = "INFO"
  }
    """

  implicit val system = ActorSystem("FacebookServer", ConfigFactory.parseString(actorConf))
  implicit val timeout = Timeout(1000000)

  val routes =
    createProfile ~
      addFriend ~
      getFrndList ~
      addPost ~
      getPage ~
      album ~
      postPhoto ~
      auth

  import jsonProtocol._

  val nrOfInstances: Int = 10
  val FBServers = system.actorOf(SmallestMailboxPool(nrOfInstances).props(Props(new FBServer())), name = "FB_Servers")

  startServer(interface = Constants.serverHost, port = Constants.serverPort) {
    routes
  }

  def jsonRes(route: Route) = {
    cookie("userIdToken") { cookieUserId =>
      respondWithMediaType(MediaTypes.`application/json`) {
        detach() {
          route
        }
      }
    }
  }

  def get_JsonRes(route: Route) = get {
    cookie("userIdToken") { cookieUserId =>
      respondWithMediaType(MediaTypes.`application/json`) {
        detach() {
          route
        }
      }
    }
  }

  def post_JsonRes(route: Route) = post {
    cookie("userIdToken") { cookieUserId =>
      respondWithMediaType(MediaTypes.`application/json`) {
        detach() {
          route
        }
      }
    }
  }

  //to be verified userId vs token number.
  var toBeVerifiedTokens = new util.HashMap[String, String]()
  //Verified userName vs public keys
  var userIdPublicKeys = new ConcurrentHashMap[String, PublicKey]()

  def decodeKey(publicKey: String): PublicKey = {
    var publicBytes = Base64.decodeBase64(publicKey);
    var keySpec = new X509EncodedKeySpec(publicBytes);
    var keyFactory = KeyFactory.getInstance("RSA");
    var pubKey = keyFactory.generatePublic(keySpec);
    pubKey
  }

  lazy val auth = {
    post {
      path("auth" / "request") {
        entity(as[AuthReq]) { auth =>
          complete {
            var uuid = UUID.randomUUID().toString()
            var encryptedContent = Security.encryptRSA(uuid, decodeKey(auth.key))
            toBeVerifiedTokens.put(auth.userId, uuid)
            encryptedContent
          }
        }
      } ~
        path("auth" / "verify") {
          entity(as[AuthToken]) { auth =>
            if (toBeVerifiedTokens.get(auth.userId).equals(auth.token)) {
              toBeVerifiedTokens.remove(auth.userId)
              setCookie(HttpCookie("userIdToken", auth.userId)) {
                println("User verified on server : " + auth.userId)
                complete("true")
              }
            } else {
              println("User rejected on server")
              complete("false")
            }
          }
        }
    }
  }

  lazy val createProfile = {
    jsonRes {
      path("user") {
        put {
          entity(as[User]) { newUsr =>
            complete {
              if (!userbase.contains(newUsr.userId)) {
                val f = Await.result(FBServers ? newUsr, timeout.duration)
                if (f.isInstanceOf[Success]) {
                  f.asInstanceOf[Success]
                }
                else if (f.isInstanceOf[Error]) {
                  f.asInstanceOf[Error]
                }
                else
                  Error("Failed due to internal error2")
              } else {
                Error("User already exists!")
              }
            }
          }
        } ~
          post {
            entity(as[User]) { newUsr =>
              complete {
                if (userbase.contains(newUsr.userId)) {
                  val f = Await.result(FBServers ? newUsr, timeout.duration)
                  if (f.isInstanceOf[Success]) {
                    f.asInstanceOf[Success]
                  }
                  else if (f.isInstanceOf[Error]) {
                    f.asInstanceOf[Error]
                  }
                  else
                    Error("Failed due to internal error2")
                } else {
                  Error("User doesnt exist!")
                }
              }
            }
          } ~
          get {
            parameters("userId") { userId =>
              complete {
                val f = Await.result(FBServers ? findProfile(userId), timeout.duration)
                if (f.isInstanceOf[User]) {
                  f.asInstanceOf[User]
                } else if (f.isInstanceOf[Error]) {
                  f.asInstanceOf[Error]
                } else {
                  Error("Failed due to internal error")
                }
              }
            }
          }
      }
    }
  }

  lazy val addPost = {
    jsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "feed") { userId =>
        put {
          //Verify the user. TODO        
          entity(as[UserPost]) { postjson =>
            complete {
              postjson.uuid = UUID.randomUUID().toString()
              val f = Await.result(FBServers ? postjson, timeout.duration)
              if (f.isInstanceOf[PostAdded]) {
                f.asInstanceOf[PostAdded]
              } else if (f.isInstanceOf[Error]) {
                f.asInstanceOf[Error]
              } else {
                Error("Failed due to internal error")
              }
            }
          }
        }
      }

    }
  }

  lazy val getPage = {
    get_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "home") { userId =>
        complete {
          var f = Await.result(FBServers ? getUserPage(userId), timeout.duration)
          if (f.isInstanceOf[UserPage]) {
            f.asInstanceOf[UserPage]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
        }
      }
    }
  }

  lazy val addFriend = {
    post_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "addfriend") { userId =>
        entity(as[FriendRequest]) { frndreq =>
          complete {
            val f = Await.result(FBServers ? frndreq, timeout.duration)
            if (f.isInstanceOf[UsersList])
              f.asInstanceOf[UsersList]
            else if (f.isInstanceOf[Error])
              f.asInstanceOf[Error]
            else
              Error("Failed due to internal error2")
          }
        }
      }
    }
  }

  lazy val getFrndList = {
    get_JsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "friendslist" / "[a-zA-Z0-9]*".r) { (userId, frndId) =>
        complete {
          var f = Await.result(FBServers ? getFriendsList(userId, frndId), timeout.duration)
          if (f.isInstanceOf[UsersList]) {
            f.asInstanceOf[UsersList]
          } else if (f.isInstanceOf[Error]) {
            f.asInstanceOf[Error]
          } else {
            Error("Failed due to internal error")
          }
        }
      }
    }
  }

  lazy val album = {
    jsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "albums") { (userId) =>
        get {
          parameters("frndId") { (frndId) =>
            complete {
              var f = Await.result(FBServers ? getUserAlbums(userId, frndId), timeout.duration)
              if (f.isInstanceOf[Array[Album]]) {
                f.asInstanceOf[Array[Album]]
              } else if (f.isInstanceOf[Error]) {
                f.asInstanceOf[Error]
              } else {
                Error("Failed due to internal error")
              }
            }
          }
        } ~
          put {
            //have to pass json but the concept remains the same. albumid = albumname
            entity(as[Album]) { album =>
              complete {
                if (userId.equals(album.userId)) {
                  var f = Await.result(FBServers ? album, timeout.duration)
                  if (f.isInstanceOf[Success]) {
                    f.asInstanceOf[Success]
                  } else if (f.isInstanceOf[Error]) {
                    f.asInstanceOf[Error]
                  } else {
                    Error("Failed due to internal error")
                  }
                } else {
                  Error(Constants.messages.noPermission)
                }
              }
            }
          } ~
          post {
            entity(as[Album]) { album =>
              complete {
                if (userId.equals(album.userId)) {
                  var f = Await.result(FBServers ? album, timeout.duration)
                  if (f.isInstanceOf[Success]) {
                    f.asInstanceOf[Success]
                  } else if (f.isInstanceOf[Error]) {
                    f.asInstanceOf[Error]
                  } else {
                    Error("Failed due to internal error")
                  }
                } else {
                  Error(Constants.messages.noPermission)
                }
              }
            }
          }
      }
    }
  }

  lazy val postPhoto = {
    jsonRes {
      path("user" / "[a-zA-Z0-9]*".r / "albums" / "photo") { userId =>
        put {
          //have to pass json but the concept remains the same. photo inside an album
          entity(as[Photo]) { photo =>
            complete {
              var f = Await.result(FBServers ? photo, timeout.duration)
              if (f.isInstanceOf[Success]) {
                f.asInstanceOf[Success]
              } else if (f.isInstanceOf[Error]) {
                f.asInstanceOf[Error]
              } else {
                Error("Failed due to internal error")
              }
            }
          }
        } ~
          get {
            parameters("photoId") { photoId =>
              complete {
                var f = Await.result(FBServers ? getPhotos(photoId), timeout.duration)
                if (f.isInstanceOf[Photo]) {
                  f.asInstanceOf[Photo]
                } else if (f.isInstanceOf[Error]) {
                  f.asInstanceOf[Error]
                } else {
                  Error("Failed due to internal error")
                }
              }
            }
          }
      }
    }
  }

  var userbase = new ParHashMap[String, UserInfo]()

  class FBServer extends Actor with ActorLogging {

    def receive = {
      case u: User => {
        var userId = u.userId
        if (!userbase.contains(userId)) {
          var user = createUserWithID(u)
          userbase.put(userId, user)
          log.info("created user : " + userId)
          sender ! Success(Constants.messages.created + userId)
        } else {
          var user = userbase.get(userId)
          user.get.insertData(u.age, u.firstName, u.lastName, u.gender, u.relation)
          sender ! Success(userId + Constants.messages.updated)
        }
      }

      case fr: FriendRequest => {
        var userId = fr.userId
        var frndIds = fr.frndId.split(" , ")

        if (userbase.contains(userId)) {
          var newIds = Array[String]()
          var user = userbase.get(userId)
          var it = frndIds.iterator
          while (it.hasNext) {
            var frndId = it.next().trim()
            log.debug("Making friends : " + userId + " , " + frndId)
            if (!frndId.isEmpty() && userId != frndId && userbase.contains(frndId)) {
              user.get.addFriend(frndId)
              var frnd = userbase.get(frndId)
              frnd.get.addFriend(userId)
            }
            newIds = newIds :+ frndId
          }
          sender ! UsersList(newIds)
        } else {
          log.error("Either of user or friend id is unavialable: " + userId + frndIds.mkString + "userbase" + userbase.size)
          sender ! Error(Constants.messages.noUser + userId + " or " + frndIds)
        }
      }

      case gpr: findProfile => {
        var user = userbase.get(gpr.userId)
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          sender ! user.get.getPublicProfile()
        }
      }

      case ap: UserPost => {
        var user = userbase.get(ap.postby)
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          //Can control this based on a private post or friends post.
          if (ap.privacy.equals(Constants.Privacy.Private)) {
            user.get.addToFeed(ap.uuid, ap)
          } else if (ap.privacy.equals(Constants.Privacy.Friends)) {
            user.get.addToFeed(ap.uuid, ap)
            var it = user.get.getFriendList().iterator
            while (it.hasNext) {
              var friendId = it.next()
              var friend = userbase.get(friendId)
              friend.get.addToFeed(ap.uuid, ap)
            }
          }
          sender ! PostAdded(ap.uuid, Constants.messages.success)
        }
      }

      case gp: getUserPage => {
        var user = userbase.get(gp.userId)
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          sender ! user.get.getPosts()
        }
      }

      case gfl: getFriendsList => {

        var user = userbase.get(gfl.userId)
        log.debug(gfl.userId + " requested list of  " + gfl.frndId + " and his friends are " + user.get.getFriendList().toList.toString())
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          if (gfl.frndId.isEmpty()) {
            // Send your own friend list
            sender ! UsersList(user.get.getFriendList().toList.toArray)
          } else {
            // Send your friends list
            if (user.get.friendList.contains(gfl.frndId) || gfl.userId.equals(gfl.frndId)) {
              var it = user.get.getFriendList().iterator
              var newIds = Array[String]()
              while (it.hasNext) {
                newIds = newIds :+ it.next()
              }
              sender ! UsersList(newIds)
            } else {
              sender ! Error(Constants.messages.noPermission)
            }
          }
        }
      }

      case aa: Album => {
        var user = userbase.get(aa.userId)

        var isSuccess = false

        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          if (user.get.getUserAlbumsIds(aa.userId).contains(aa.albumId)) {
            isSuccess = user.get.updateAlbumToUser(aa.userId, aa)
            if (isSuccess) {
              //  Add this album permission to friends TODO
              sender ! Success(Constants.messages.albumUpdated + aa.albumId)
            } else {
              sender ! Error(Constants.messages.noAlbum)
            }
          } else {
            isSuccess = user.get.addAlbumToUser(aa.userId, aa)
            if (isSuccess) {
              //  Add this album permission to friends TODO
              sender ! Success(Constants.messages.albumCreated + aa.albumId)
            } else {
              sender ! Error(Constants.messages.albumCreationFailed)
            }
          }

        }
      }

      case da: deleteAlbum => {
        var user = userbase.get(da.userId)
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          sender ! user.get.deleteAlbumToUser(da.userId, da.albumId)
        }
      }

      case gai: getUserAlbums => {
        var user = userbase.get(gai.userId)
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          if (gai.userId.equals(gai.frndId)) {
            sender ! user.get.getUserAlbums()
          } else if (userbase.contains(gai.frndId)) {
            sender ! userbase.get(gai.frndId).get.getUserAlbums()
          }
        }
      }

      case ai: Photo => {
        var user = userbase.get(ai.userId)
        if (user.isEmpty) {
          sender ! Error(Constants.messages.noUser)
        } else {
          var isSuccess = user.get.addPhotoToAlbum(ai.userId, ai.albumId, ai)
          if (isSuccess) {
            var p = UserPost(ai.photoId, ai.userId, ai.src, ai.message, ai.place, Constants.Privacy.Friends, Some(ai.photoId), Constants.PostTypes.Photo)
            user.get.addToFeed(ai.photoId, p)
            if (!ai.noStory) {
              // If this is a personal post. Just add to the feed of the user. 
              // Else Add this photo permission to friends 
              var it = user.get.getFriendList().iterator
              while (it.hasNext) {
                var friendId = it.next()
                var friend = userbase.get(friendId)
                friend.get.addToFeed(ai.photoId, p)
              }
            }
            sender ! Success(Constants.messages.photoAdded + ai.photoId)
          } else {
            sender ! Error(Constants.messages.photoAddFailed)
          }
        }
      }

      case gp: getPhotos => {
        var user = userbase.get(gp.photoId)
        if (user.isEmpty || gp.photoId.isEmpty()) {
          sender ! Error(Constants.messages.noUser)
        } else {
          sender ! user.get.getUserAlbumPhoto(user.get.userId, "", gp.photoId)
        }
      }
    }

    //Helper methods
    def createUserWithID(u: User): UserInfo = {
      var user = new UserInfo(u.userId)
      user.insertData(u.age, u.firstName, u.lastName, u.gender, u.relation)
      user
    }

  }

  class UserInfo(val userId: String, var age: Int = -1, var firstName: String = "", var lastName: String = "", var gender: String = "NA", var relation: String = "") {

    var posts = new ListBuffer[UserPost]()
    var friendList = new ListBuffer[String]()
    val log = Logger(LoggerFactory.getLogger("UserInfo-" + userId))

    private var userAlbums = new ListBuffer[String]
    private var albumStore = new HashMap[String, PictureAlbum]
    private var photoStore = new HashMap[String, Picture]

    def insertData(a: Int, fn: String, ln: String, gen: String, rel: String) {
      age = a
      firstName = fn
      lastName = ln
      gender = gen
      relation = rel
    }

    def addToFeed(postId: String, post: UserPost) = {
      log.debug(userId + " addToFeed " + post + posts.length)
      posts.prepend(post)
    }

    def getPosts(): UserPage = {
      log.debug(userId + " getPosts " + posts.length)
      return UserPage(posts.toList.toArray)
    }

    def addFriend(friendId: String) = {
      log.debug(userId + " addFriend " + friendId)
      friendList.append(friendId)
    }

    def getPublicProfile(): User = {
      User(userId, firstName, lastName, age, gender, relation)
    }

    def getFriendList(): ListBuffer[String] = {
      friendList
    }

    def addAlbumToUser(userId: String, album: Album): Boolean = {
      if (userAlbums.contains(album.albumId)) {
        log.error("Album already added!!")
        false
      } else {
        userAlbums.append(album.albumId)
        var picAlbum = new PictureAlbum(userId, album.albumId)
        picAlbum.populate(album.coverPhoto, album.description, album.place)
        albumStore.put(album.albumId, picAlbum)
        true
      }
    }

    def updateAlbumToUser(userId: String, album: Album): Boolean = {
      if (userAlbums.contains(album.albumId)) {
        var picAlbum = albumStore.get(album.albumId)
        picAlbum.update(album.coverPhoto, album.description, album.place)
        true
      } else {
        log.error("Album doesnot exist!!")
        false
      }
    }

    def deleteAlbumToUser(userId: String, albumId: String): Boolean = {
      if (userAlbums.contains(albumId)) {
        userAlbums -= albumId
        albumStore.remove(albumId)
        true
      } else {
        log.error("Album doesnot exist!!")
        false
      }
    }

    def addPhotoToAlbum(userId: String, albumId: String, photo: Photo): Boolean = {
      if (photoStore.get(photo.photoId) == null) {
        if (albumStore.get(albumId) == null) {
          log.error("Album not found")
          false
        } else {
          var album: PictureAlbum = albumStore.get(albumId)
          var pic = new Picture(albumId, photo.photoId, photo.src)
          pic.populate(photo.message, photo.place)
          album.addPicture(pic)
          photoStore.put(pic.photoId, pic)
          true
        }
      } else {
        log.error("Photo already exists!!")
        false
      }
    }

    def getUserAlbumsIds(userId: String): ListBuffer[String] = {
      userAlbums
    }

    def getAlbumInfo(albumId: String): Album = {
      var a = albumStore.get(albumId)
      var b = new Album(a.ownerId, a.albumId, a.coverPhoto, Option(a.createdTime), a.description, a.place, Option(a.updateTime), Option(a.photos.toList.toArray))
      b
    }

    def getUserAlbums(): Array[Album] = {
      var a = userAlbums
      var it = a.iterator
      var albums = Array[Album]()
      while (it.hasNext) {
        var albumId = it.next()
        if (albumId != null)
          albums = albums :+ getAlbumInfo(albumId)
      }
      albums
    }

    def getUserAlbumPhotos(userId: String, albumId: String): Array[Photo] = {
      var a = albumStore.get(albumId)
      var it = a.photos.iterator
      var pics = Array[Photo]()
      while (it.hasNext) {
        var picId = it.next()
        if (picId != null)
          pics = pics :+ getUserAlbumPhoto(userId, albumId, picId)
      }
      pics
    }

    def getUserAlbumPhoto(userId: String, albumId: String, photoId: String): Photo = {
      var p = photoStore.get(photoId)
      new Photo(userId, albumId, p.photoId, p.src, p.message, p.place, p.nostory)
    }

  }

}